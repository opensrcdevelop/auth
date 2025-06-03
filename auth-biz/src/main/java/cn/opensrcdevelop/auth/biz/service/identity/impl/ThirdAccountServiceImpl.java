package cn.opensrcdevelop.auth.biz.service.identity.impl;

import cn.opensrcdevelop.auth.biz.dto.identity.UserBindingResponseDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.entity.identity.ThirdAccount;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.mapper.identity.ThirdAccountMapper;
import cn.opensrcdevelop.auth.biz.repository.identity.ThirdAccountRepository;
import cn.opensrcdevelop.auth.biz.service.identity.ThirdAccountService;
import cn.opensrcdevelop.auth.biz.service.user.impl.UserServiceImpl;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.vavr.control.Try;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.OAuth2Error;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
@Service
@RequiredArgsConstructor
public class ThirdAccountServiceImpl extends ServiceImpl<ThirdAccountMapper, ThirdAccount> implements ThirdAccountService {

    private static final String INVALID_USER_INFO_RESPONSE_ERROR_CODE = "invalid_user_info_response";
    private static final Integer USERNAME_LENGTH = 8;
    private static final String EMAIL_ATTR = "email";

    private final UserServiceImpl userService;
    private final ThirdAccountRepository thirdAccountRepository;

    @Transactional
    @Override
    public User bind(List<Map<String, Object>> attributesList, IdentitySourceRegistration identitySourceRegistration) {

        String usernameAttribute = identitySourceRegistration.getIdentitySourceProvider().getUsernameAttribute();
        String usernameAttributeValue = getAttribute(attributesList, usernameAttribute);
        if (StringUtils.isEmpty(usernameAttributeValue)) {
            log.warn("第三方用户信息中未找到用户名属性 {} 或该用户属性值为 null，无法绑定身份源 {}", usernameAttribute, identitySourceRegistration.getRegistrationId());
            OAuth2Error oauth2Error = new OAuth2Error(INVALID_USER_INFO_RESPONSE_ERROR_CODE);
            throw new OAuth2AuthenticationException(oauth2Error, INVALID_USER_INFO_RESPONSE_ERROR_CODE);
        }

        // 1. 检查第三方账号是否已绑定
        ThirdAccount boundThirdAccount = super.getOne(Wrappers.<ThirdAccount>lambdaQuery()
                .eq(ThirdAccount::getUniqueId, usernameAttributeValue)
                .eq(ThirdAccount::getRegistrationId, identitySourceRegistration.getRegistrationId()));
        if (Objects.nonNull(boundThirdAccount)) {
            // 2. 已绑定，更新第三方用户信息
            super.update(Wrappers.<ThirdAccount>lambdaUpdate()
                    .set(ThirdAccount::getDetails, CommonUtil.serializeObject(attributesList))
                    .eq(ThirdAccount::getUniqueId, usernameAttributeValue)
                    .eq(ThirdAccount::getRegistrationId, identitySourceRegistration.getRegistrationId()));
            // 2.1 返回绑定的用户
            return userService.getById(boundThirdAccount.getUserId());
        } else {
            // 3. 未绑定，绑定第三方账号
            // 3.1 获取用户匹配属性
            String userMatchAttribute = identitySourceRegistration.getIdentitySourceProvider().getUserMatchAttribute();
            String userMatchAttributeValue = getAttribute(attributesList, userMatchAttribute);
            if (StringUtils.isEmpty(userMatchAttributeValue)) {
                log.warn("用户信息中未找到匹配的用户属性 {} 或该用户属性值为 null，无法绑定身份源 {}", userMatchAttribute, identitySourceRegistration.getRegistrationId());
                OAuth2Error oauth2Error = new OAuth2Error(INVALID_USER_INFO_RESPONSE_ERROR_CODE);
                throw new OAuth2AuthenticationException(oauth2Error, INVALID_USER_INFO_RESPONSE_ERROR_CODE);
            }

            // 3.2 获取用户
            User user = Try.of(() -> (User) userService.loadUserByUsername(userMatchAttributeValue)).getOrElse(() -> null);
            if (Objects.isNull(user)) {
                // 3.2.1 未找到用户，自动注册
                String randomUsername = CommonUtil.generateRandomString(USERNAME_LENGTH);
                User newUser = new User();
                newUser.setUserId(CommonUtil.getUUIDString());
                newUser.setUsername(randomUsername);
                if (EMAIL_ATTR.equals(userMatchAttribute)) {
                    newUser.setEmailAddress(userMatchAttributeValue);
                }
                userService.save(newUser);
                user = newUser;
                log.info("未找到匹配的用户 {}，已自动注册用户 {}", userMatchAttributeValue, randomUsername);
            }
            // 3.3 绑定第三方账号
            ThirdAccount thirdAccount = new ThirdAccount();
            thirdAccount.setUserId(user.getUserId());
            thirdAccount.setUniqueId(usernameAttributeValue);
            thirdAccount.setRegistrationId(identitySourceRegistration.getRegistrationId());
            thirdAccount.setDetails(CommonUtil.serializeObject(attributesList));
            super.save(thirdAccount);

            // 3.4 返回绑定的用户
            return user;
        }
    }

    /**
     * 获取用户绑定列表
     *
     * @param registrationId 注册身份源ID
     * @param page 页数
     * @param size 条数
     * @param keyword 用户名检索关键字
     * @return 用户绑定列表
     */
    @Override
    public PageData<UserBindingResponseDto> getUserBindingList(String registrationId, int page, int size, String keyword) {
        // 1. 创建分页对象
        Page<ThirdAccount> pageRequest = new Page<>(page, size);
        PageData<UserBindingResponseDto> pageData = new PageData<>();

        // 2. 查询数据库
        thirdAccountRepository.searchUserBindings(pageRequest, registrationId, keyword);
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());

        // 3. 转换为响应对象
        var responseDtoList = CommonUtil.stream(pageRequest.getRecords()).map(thirdAccount ->
                UserBindingResponseDto.builder()
                        .userId(thirdAccount.getUser().getUserId())
                        .username(thirdAccount.getUser().getUsername())
                        .uniqueId(thirdAccount.getUniqueId())
                        .bindingTime(thirdAccount.getCreateTime())
                        .build()
        ).toList();
        pageData.setList(responseDtoList);
        return pageData;
    }

    @SuppressWarnings("all")
    private String getAttribute(List<Map<String, Object>> attributesList, String attributeName) {
        for (Map<String, Object> attributes : attributesList) {
            if (attributes.containsKey(attributeName)) {
                return attributes.get(attributeName).toString();
            } else {
                var entrySet = attributes.entrySet();
                for (var entry : entrySet) {
                    var v = entry.getValue();
                    if (v instanceof Map mapAttribute) {
                        return getAttribute(List.of(mapAttribute), attributeName);
                    }

                    if (v instanceof List listAttribute) {
                        return getAttribute(listAttribute, attributeName);
                    }
                }

                return null;
            }
        }
        return null;
    }
}
