package cn.opensrcdevelop.auth.biz.service.identity.impl;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceProviderResponseDto;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceRegistrationRequestDto;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceRegistrationResponseDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceProvider;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.entity.identity.ThirdAccount;
import cn.opensrcdevelop.auth.biz.mapper.identity.IdentitySourceRegistrationMapper;
import cn.opensrcdevelop.auth.biz.repository.identity.IdentitySourceRegistrationRepository;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.auth.biz.service.identity.ThirdAccountService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class IdentitySourceRegistrationServiceImpl extends ServiceImpl<IdentitySourceRegistrationMapper, IdentitySourceRegistration> implements IdentitySourceRegistrationService {

    private static final String AUTHORIZATION_URI_FORMAT = "%s" + AuthConstants.FEDERATION_LOGIN_URI + "/%s";
    private final IdentitySourceRegistrationRepository identitySourceRegistrationRepository;
    private final ThirdAccountService thirdAccountService;

    /**
     * 根据标识获取注册信息
     *
     * @param code 标识
     * @return 注册信息
     */
    @Override
    public IdentitySourceRegistration getByCode(String code) {
        return identitySourceRegistrationRepository.getRegistrationByCode(code);
    }

    /**
     * 批量删除注册信息
     *
     * @param ids 注册 ID 列表
     */
    @Transactional
    @Override
    public void removeIdentitySourceRegistrations(List<String> ids) {
        // 1. 删除绑定的第三方账号
        thirdAccountService.remove(Wrappers.<ThirdAccount>lambdaQuery().in(ThirdAccount::getRegistrationId, ids));

        // 2. 删除注册信息
        super.removeBatchByIds(ids);
    }

    /**
     * 注册身份源
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createIdentitySourceRegistration(IdentitySourceRegistrationRequestDto requestDto) {
        // 1. 检查身份源注册标识是否重复
        checkRegistrationCode(requestDto, null);

        // 2. 属性编辑
        IdentitySourceRegistration registration = new IdentitySourceRegistration();
        registration.setProviderId(requestDto.getProviderId());
        registration.setRegistrationId(CommonUtil.getUUIDString());
        registration.setRegistrationCode(requestDto.getCode());
        registration.setRegistrationName(requestDto.getName());
        registration.setClientId(requestDto.getClientId());
        registration.setClientSecret(requestDto.getClientSecret());
        registration.setClientAuthenticationMethod(requestDto.getClientAuthenticationMethod());
        registration.setAuthorizationGrantType(requestDto.getAuthorizationGrantType());
        registration.setEnabled(Boolean.TRUE);
        if (MapUtils.isNotEmpty(requestDto.getAdditionalParams())) {
            registration.setAdditionalParams(CommonUtil.serializeObject(requestDto.getAdditionalParams()));
        }

        // 3. 数据库操作
        super.save(registration);
    }

    /**
     * 更新身份源注册信息
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void updateIdentitySourceRegistration(IdentitySourceRegistrationRequestDto requestDto) {
        // 1. 获取版本号
        var rawRegistration = super.getById(requestDto.getId());
        if (Objects.isNull(rawRegistration)) {
            return;
        }

        // 2. 检查身份源注册标识是否重复
        checkRegistrationCode(requestDto, rawRegistration);

        // 3. 属性编辑
        IdentitySourceRegistration updateRegistration = new IdentitySourceRegistration();
        updateRegistration.setRegistrationId(requestDto.getId());
        updateRegistration.setRegistrationCode(requestDto.getCode());
        updateRegistration.setRegistrationName(requestDto.getName());
        updateRegistration.setClientId(requestDto.getClientId());
        updateRegistration.setClientSecret(requestDto.getClientSecret());
        updateRegistration.setClientAuthenticationMethod(requestDto.getClientAuthenticationMethod());
        updateRegistration.setAuthorizationGrantType(requestDto.getAuthorizationGrantType());
        CommonUtil.callSetWithCheck(Objects::nonNull, updateRegistration::setEnabled, requestDto::getEnabled);

        if (MapUtils.isNotEmpty(requestDto.getAdditionalParams())) {
            updateRegistration.setAdditionalParams(CommonUtil.formatJson(requestDto.getAdditionalParams()));
        }

        // 4. 数据库操作
        super.updateById(updateRegistration);
    }

    /**
     * 删除身份源注册信息
     *
     * @param id 注册 ID
     */
    @Override
    public void removeIdentitySourceRegistration(String id) {
        // 1. 删除绑定的第三方账号
        thirdAccountService.remove(Wrappers.<ThirdAccount>lambdaQuery().eq(ThirdAccount::getRegistrationId, id));

        // 2. 删除注册信息
        super.removeById(id);
    }

    /**
     * 获取身份源注册列表
     *
     * @param page 页数
     * @param size 条数
     * @param keyword 身份源注册标识或名称检索关键字
     * @return 身份源注册列表
     */
    @Override
    public PageData<IdentitySourceRegistrationResponseDto> list(int page, int size, String keyword) {
        // 1. 数据库操作
        Page<IdentitySourceRegistration> pageRequest = new Page<>(page, size);
        identitySourceRegistrationRepository.searchRegistrations(pageRequest, keyword);

        // 2. 属性设置
        PageData<IdentitySourceRegistrationResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var records = CommonUtil.stream(pageRequest.getRecords()).map(registration -> {
            // 提供商
            IdentitySourceProvider identitySourceProvider = registration.getIdentitySourceProvider();
            IdentitySourceProviderResponseDto provider = IdentitySourceProviderResponseDto.builder()
                    .id(identitySourceProvider.getProviderId())
                    .name(identitySourceProvider.getProviderName())
                    .code(identitySourceProvider.getProviderCode())
                    .build();

            return IdentitySourceRegistrationResponseDto.builder()
                    .id(registration.getRegistrationId())
                    .code(registration.getRegistrationCode())
                    .name(registration.getRegistrationName())
                    .enabled(registration.getEnabled())
                    .provider(provider)
                    .build();
        }).toList();
        pageData.setList(records);
        return pageData;
    }

    /**
     * 获取身份源注册信息详情
     *
     * @param id 注册 ID
     * @return 身份源注册信息详情
     */
    @Override
    public IdentitySourceRegistrationResponseDto detail(String id) {
        // 1. 数据库操作
        IdentitySourceRegistration registration = super.getById(id);
        if (Objects.isNull(registration)) {
            return IdentitySourceRegistrationResponseDto.builder().build();
        }

        // 2. 属性设置
        return IdentitySourceRegistrationResponseDto.builder()
                .id(registration.getRegistrationId())
                .code(registration.getRegistrationCode())
                .name(registration.getRegistrationName())
                .clientId(registration.getClientId())
                .clientSecret(registration.getClientSecret())
                .clientAuthenticationMethod(registration.getClientAuthenticationMethod())
                .authorizationGrantType(registration.getAuthorizationGrantType())
                .enabled(registration.getEnabled())
                .additionalParams(registration.getAdditionalParams())
                .build();
    }

    /**
     * 获取启用的注册身份源
     *
     * @return 启用的注册身份源
     */
    @Override
    public List<IdentitySourceRegistrationResponseDto> getEnabledRegistrations() {
        // 1. 数据库操作
        List<IdentitySourceRegistration> registrations = identitySourceRegistrationRepository.getEnabledRegistrations();

        // 2. 属性设置
        return CommonUtil.stream(registrations).map(registration -> {
            String code = registration.getRegistrationCode();
            String authorizationUri = String.format(AUTHORIZATION_URI_FORMAT, WebUtil.getRootUrl(), code);
            return IdentitySourceRegistrationResponseDto.builder()
                    .name(registration.getRegistrationName())
                    .code(code)
                    .logo(registration.getIdentitySourceProvider().getProviderLogo())
                    .authorizationUri(authorizationUri)
                    .build();
        }).toList();
    }

    private void checkRegistrationCode(IdentitySourceRegistrationRequestDto requestDto, IdentitySourceRegistration rawRegistration) {
        if (Objects.nonNull(rawRegistration) && StringUtils.equals(requestDto.getCode(), rawRegistration.getRegistrationCode())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<IdentitySourceRegistration>lambdaQuery().eq(IdentitySourceRegistration::getRegistrationCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.IDENTITY_REGISTRATION_MSG_1000, requestDto.getCode());
        }
    }
}
