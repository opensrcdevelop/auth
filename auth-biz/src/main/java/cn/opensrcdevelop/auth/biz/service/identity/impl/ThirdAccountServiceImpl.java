package cn.opensrcdevelop.auth.biz.service.identity.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.dto.identity.UserBindingResponseDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceProvider;
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
import io.vavr.Tuple;
import io.vavr.Tuple2;
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

    /**
     * 绑定第三方账号
     *
     * @param attributesList 第三方用户信息
     * @param identitySourceRegistration 身份源
     * @return 用户
     */
    @Transactional
    @Override
    public User bind(List<Map<String, Object>> attributesList, IdentitySourceRegistration identitySourceRegistration) {

        var checkRes = doCheck(attributesList, identitySourceRegistration);
        String usernameAttributeValue = checkRes._1;
        String uniqueIdAttributeValue = checkRes._2;

        // 1. 检查第三方账号是否已绑定
        ThirdAccount boundThirdAccount = super.getOne(Wrappers.<ThirdAccount>lambdaQuery()
                .eq(ThirdAccount::getUniqueId, uniqueIdAttributeValue)
                .eq(ThirdAccount::getRegistrationId, identitySourceRegistration.getRegistrationId()));
        if (Objects.nonNull(boundThirdAccount)) {
            // 2. 已绑定
            User user =userService.getById(boundThirdAccount.getUserId());
            // 2.1 检查绑定的用户的状态
            checkAccountStatus(user);

            // 2.2 更新第三方用户信息
            super.update(Wrappers.<ThirdAccount>lambdaUpdate()
                    .set(ThirdAccount::getUsername, usernameAttributeValue)
                    .set(ThirdAccount::getDetails, CommonUtil.serializeObject(attributesList))
                    .eq(ThirdAccount::getUniqueId, uniqueIdAttributeValue)
                    .eq(ThirdAccount::getRegistrationId, identitySourceRegistration.getRegistrationId()));
            // 2.3 返回绑定的用户
            return user;
        } else {
            // 3. 未绑定，绑定第三方账号
            // 3.1 获取用户匹配属性
            String userMatchAttribute = identitySourceRegistration.getIdentitySourceProvider().getUserMatchAttribute();
            String userMatchAttributeValue = getAttribute(attributesList, userMatchAttribute);
            if (StringUtils.isEmpty(userMatchAttributeValue)) {
                log.warn("用户信息中未找到有效的用户匹配属性 {}，无法绑定身份源 {}", userMatchAttribute, identitySourceRegistration.getRegistrationId());
                OAuth2Error oauth2Error = new OAuth2Error(INVALID_USER_INFO_RESPONSE_ERROR_CODE);
                throw new OAuth2AuthenticationException(oauth2Error, INVALID_USER_INFO_RESPONSE_ERROR_CODE);
            }

            // 3.2 获取用户
            User user = Try.of(() -> (User) userService.loadUserByUsername(userMatchAttributeValue)).getOrElse(() -> null);
            if (Objects.isNull(user)) {
                // 3.2.1 未找到用户，自动注册
                String randomUsername = CommonUtil.generateRandomString(USERNAME_LENGTH);
                User newUser = new User();
                newUser.setUserId(CommonUtil.getUUIDV7String());
                newUser.setUsername(randomUsername);
                if (EMAIL_ATTR.equals(userMatchAttribute)) {
                    newUser.setEmailAddress(userMatchAttributeValue);
                }
                userService.save(newUser);
                user = newUser;
                log.info("未找到匹配的用户 {}，已自动注册用户 {}", userMatchAttributeValue, randomUsername);
            }
            // 3.3 绑定第三方账号
            // 3.3.1 检查绑定的用户的状态
            checkAccountStatus(user);

            // 3.3.2 绑定第三方账号
            ThirdAccount thirdAccount = new ThirdAccount();
            thirdAccount.setUserId(user.getUserId());
            thirdAccount.setUniqueId(uniqueIdAttributeValue);
            thirdAccount.setUsername(usernameAttributeValue);
            thirdAccount.setRegistrationId(identitySourceRegistration.getRegistrationId());
            thirdAccount.setDetails(CommonUtil.serializeObject(attributesList));
            super.save(thirdAccount);

            // 3.4 返回绑定的用户
            return user;
        }
    }

    /**
     * 用户自主绑定第三方账号
     *
     * @param userId 用户ID
     * @param attributesList 第三方用户信息
     * @param identitySourceRegistration 身份源
     * @return 用户
     */
    @Audit(
            userId = "#userId",
            type = AuditType.USER_OPERATION,
            resource = ResourceType.IDENTITY_SOURCE,
            userOperation = UserOperationType.BIND_THIRD_ACCOUNT,
            success = "'绑定了身份源（' + @linkGen.toLink(#identitySourceRegistration.registrationId, T(ResourceType).IDENTITY_SOURCE) + '）'",
            error = "'绑定身份源失败（' + @linkGen.toLink(#identitySourceRegistration.registrationId, T(ResourceType).IDENTITY_SOURCE) + '）'"
    )
    @Transactional
    @Override
    public User bind(String userId, List<Map<String, Object>> attributesList, IdentitySourceRegistration identitySourceRegistration) {
        var checkRes = doCheck(attributesList, identitySourceRegistration);
        String usernameAttributeValue = checkRes._1;
        String uniqueIdAttributeValue = checkRes._2;

        // 1. 检查第三方账号是否已绑定
        ThirdAccount boundThirdAccount = super.getOne(Wrappers.<ThirdAccount>lambdaQuery()
                .eq(ThirdAccount::getUniqueId, uniqueIdAttributeValue)
                .eq(ThirdAccount::getRegistrationId, identitySourceRegistration.getRegistrationId()));

        // 2. 第三方账号已被绑定
        if (Objects.nonNull(boundThirdAccount)) {
            OAuth2Error oauth2Error = new OAuth2Error(AuthConstants.THIRD_ACCOUNT_ALREADY_EXISTS_ERROR_CODE);
            throw new OAuth2AuthenticationException(oauth2Error, AuthConstants.THIRD_ACCOUNT_ALREADY_EXISTS_ERROR_CODE);
        }

        // 3. 第三方账号未被绑定，绑定第三方账号
        ThirdAccount thirdAccount = new ThirdAccount();
        thirdAccount.setUserId(userId);
        thirdAccount.setUniqueId(uniqueIdAttributeValue);
        thirdAccount.setUsername(usernameAttributeValue);
        thirdAccount.setRegistrationId(identitySourceRegistration.getRegistrationId());
        thirdAccount.setDetails(CommonUtil.serializeObject(attributesList));
        super.save(thirdAccount);

        return userService.getById(userId);
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
                Object value = attributes.get(attributeName);
                if (Objects.nonNull(value)) {
                    return value.toString();
                }
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
            }
        }
        return null;
    }

    private Tuple2<String, String> doCheck(List<Map<String, Object>> attributesList, IdentitySourceRegistration identitySourceRegistration) {
        IdentitySourceProvider identitySourceProvider = identitySourceRegistration.getIdentitySourceProvider();
        String usernameAttribute = identitySourceProvider.getUsernameAttribute();
        String uniqueIdAttribute = identitySourceProvider.getUniqueIdAttribute();
        String usernameAttributeValue = getAttribute(attributesList, usernameAttribute);
        String uniqueIdAttributeValue = getAttribute(attributesList, uniqueIdAttribute);

        if (StringUtils.isEmpty(usernameAttributeValue)) {
            log.warn("第三方用户信息中未找到有效的用户名属性 {}，无法绑定身份源 {}", usernameAttribute, identitySourceRegistration.getRegistrationId());
            OAuth2Error oauth2Error = new OAuth2Error(INVALID_USER_INFO_RESPONSE_ERROR_CODE);
            throw new OAuth2AuthenticationException(oauth2Error, INVALID_USER_INFO_RESPONSE_ERROR_CODE);
        }

        if (StringUtils.isEmpty(uniqueIdAttributeValue)) {
            log.warn("第三方用户信息中未找到有效的唯一标识属性 {}，无法绑定身份源 {}", uniqueIdAttribute, identitySourceRegistration.getRegistrationId());
            OAuth2Error oauth2Error = new OAuth2Error(INVALID_USER_INFO_RESPONSE_ERROR_CODE);
            throw new OAuth2AuthenticationException(oauth2Error, INVALID_USER_INFO_RESPONSE_ERROR_CODE);
        }

        return Tuple.of(usernameAttributeValue, uniqueIdAttributeValue);
    }

    private void checkAccountStatus(User user) {
        if (Boolean.TRUE.equals(user.getLocked())) {
            throw new OAuth2AuthenticationException(new OAuth2Error(AuthConstants.USER_LOCKED_ERROR_CODE), AuthConstants.USER_LOCKED_ERROR_CODE);
        }
    }
}
