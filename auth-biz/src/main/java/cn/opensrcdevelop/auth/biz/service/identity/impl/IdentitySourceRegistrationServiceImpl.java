package cn.opensrcdevelop.auth.biz.service.identity.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.auth.biz.component.CustomOAuth2AuthorizationRequestResolver;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceProviderResponseDto;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceRegistrationRequestDto;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceRegistrationResponseDto;
import cn.opensrcdevelop.auth.biz.dto.identity.UserBindingResponseDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceProvider;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.entity.identity.ThirdAccount;
import cn.opensrcdevelop.auth.biz.mapper.identity.IdentitySourceRegistrationMapper;
import cn.opensrcdevelop.auth.biz.repository.identity.IdentitySourceRegistrationRepository;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.auth.biz.service.identity.ThirdAccountService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.oauth2.client.web.AuthorizationRequestRepository;
import org.springframework.security.oauth2.client.web.HttpSessionOAuth2AuthorizationRequestRepository;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class IdentitySourceRegistrationServiceImpl extends ServiceImpl<IdentitySourceRegistrationMapper, IdentitySourceRegistration> implements IdentitySourceRegistrationService {

    private static final String AUTHORIZATION_URI_FORMAT = "%s" + AuthConstants.FEDERATION_LOGIN_URI + "/%s";

    private final IdentitySourceRegistrationRepository identitySourceRegistrationRepository;
    private final ThirdAccountService thirdAccountService;
    private final AuthorizationRequestRepository<OAuth2AuthorizationRequest> authorizationRequestRepository = new HttpSessionOAuth2AuthorizationRequestRepository();


    @Resource
    @Lazy
    private CustomOAuth2AuthorizationRequestResolver authorizationRequestResolver;

    /**
     * 根据标识获取身份源信息
     *
     * @param code 标识
     * @return 身份源信息
     */
    @Override
    public IdentitySourceRegistration getByCode(String code) {
        return identitySourceRegistrationRepository.getRegistrationByCode(code);
    }

    /**
     * 批量删除身份源
     *
     * @param ids 身份源 ID 列表
     */
    @Transactional
    @Override
    public void removeIdentitySourceRegistrations(List<String> ids) {
        // 1. 删除绑定的第三方账号
        thirdAccountService.remove(Wrappers.<ThirdAccount>lambdaQuery().in(ThirdAccount::getRegistrationId, ids));

        // 2. 删除身份源
        super.removeBatchByIds(ids);
    }

    /**
     * 创建身份源
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.IDENTITY_SOURCE,
            sysOperation = SysOperationType.CREATE,
            success = "创建了身份源（{{ #registrationId }}）",
            fail = "创建身份源（{{ #requestDto.name }}）失败，对应的提供商为：{{ @linkGen.toLink(#requestDto.providerId, T(ResourceType).IDENTITY_SOURCE_PROVIDER) }}"
    )
    @Transactional
    @Override
    public void createIdentitySourceRegistration(IdentitySourceRegistrationRequestDto requestDto) {
        // 1. 检查身份源标识是否重复
        checkRegistrationCode(requestDto, null);

        // 2. 属性编辑
        String registrationId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("registrationId", registrationId);

        IdentitySourceRegistration registration = new IdentitySourceRegistration();
        registration.setProviderId(requestDto.getProviderId());
        registration.setRegistrationId(registrationId);
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
     * 更新身份源信息
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.IDENTITY_SOURCE,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了身份源（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).IDENTITY_SOURCE) }}）",
            fail = "修改身份源（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).IDENTITY_SOURCE) }}）失败"
    )
    @Transactional
    @Override
    public void updateIdentitySourceRegistration(IdentitySourceRegistrationRequestDto requestDto) {
        String registrationId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        var rawRegistration = super.getById(requestDto.getId());
        if (Objects.isNull(rawRegistration)) {
            return;
        }
        compareObjBuilder.id(registrationId);
        compareObjBuilder.before(rawRegistration);

        // 2. 检查身份源标识是否重复
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

        compareObjBuilder.after(super.getById(registrationId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 删除身份源
     *
     * @param id 身份源 ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.IDENTITY_SOURCE,
            sysOperation = SysOperationType.DELETE,
            success = "删除了身份源（{{ @linkGen.toLink(#id, T(ResourceType).IDENTITY_SOURCE) }}）",
            fail = "删除身份源（{{ @linkGen.toLink(#id, T(ResourceType).IDENTITY_SOURCE) }}）失败"
    )
    @Override
    public void removeIdentitySourceRegistration(String id) {
        // 1. 删除绑定的第三方账号
        thirdAccountService.remove(Wrappers.<ThirdAccount>lambdaQuery().eq(ThirdAccount::getRegistrationId, id));

        // 2. 删除注册信息
        super.removeById(id);
    }

    /**
     * 获取身份源列表
     *
     * @param page 页数
     * @param size 条数
     * @param keyword 身份源标识或名称检索关键字
     * @return 身份源列表
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
     * 获取身份源详情
     *
     * @param id 注册 ID
     * @return 身份源详情
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
     * 获取启用的身份源
     *
     * @return 启用的身份源
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
                    .id(registration.getRegistrationId())
                    .name(registration.getRegistrationName())
                    .code(code)
                    .logo(registration.getIdentitySourceProvider().getProviderLogo())
                    .authorizationUri(authorizationUri)
                    .build();
        }).toList();
    }

    /**
     * 获取绑定的身份源
     *
     * @return 绑定的身份源
     */
    @Override
    public List<IdentitySourceRegistrationResponseDto> getBoundRegistrations() {
        // 1. 获取当前用户 ID
        String userId = AuthUtil.getCurrentUserId();

        // 2. 获取启用的身份源
        List<IdentitySourceRegistrationResponseDto> registrationResponseList = getEnabledRegistrations();

        // 3. 获取用户已绑定的身份源ID
        List<ThirdAccount> thirdAccounts = thirdAccountService.list(Wrappers.<ThirdAccount>lambdaQuery().eq(ThirdAccount::getUserId, userId));

        // 4. 组装返回结果
        CommonUtil.stream(registrationResponseList).forEach(registrationRep -> CommonUtil.stream(thirdAccounts)
                .filter(thirdAccount -> StringUtils.equals(registrationRep.getId(), thirdAccount.getRegistrationId()))
                .findAny().ifPresent(thirdAccount -> {
                    registrationRep.setAuthorizationUri(null);
                    registrationRep.setIsBind(true);
                    registrationRep.setBindUsername(thirdAccount.getUsername());
                }));

        return new ArrayList<>(registrationResponseList);
    }

    /**
     * 绑定用户
     *
     * @param registrationCode 身份源标识
     * @param request 请求
     * @param response 响应
     */
    @Override
    public UserBindingResponseDto bindUser(String registrationCode, HttpServletRequest request, HttpServletResponse response) throws IOException {
        UserBindingResponseDto responseDto = UserBindingResponseDto.builder().build();
        // 1. 检查身份源是否存在
        IdentitySourceRegistration registration = super.getOne(Wrappers.<IdentitySourceRegistration>lambdaQuery().eq(IdentitySourceRegistration::getRegistrationCode, registrationCode));
        if (Objects.isNull(registration)) {
            throw new BizException(MessageConstants.IDENTITY_REGISTRATION_MSG_1001, registrationCode);
        }

        // 2. 获取当前用户ID
        String userId = AuthUtil.getCurrentUserId();

        // 3. 授权请求转换保存
        OAuth2AuthorizationRequest authorizationRequest = authorizationRequestResolver.resolve(request, registrationCode);
        authorizationRequestRepository.saveAuthorizationRequest(authorizationRequest, request, response);
        request.getSession().setAttribute(AuthConstants.SESSION_BIND_REQ_USER_ID, userId);

        // 4. 返回授权重定向地址
        responseDto.setAuthReqUri(authorizationRequest.getAuthorizationRequestUri());
        return responseDto;
    }

    /**
     * 解绑用户
     *
     * @param registrationId 身份源ID
     */
    @Audit(
            type = AuditType.USER_OPERATION,
            resource = ResourceType.IDENTITY_SOURCE,
            userOperation = UserOperationType.UNBIND_THIRD_ACCOUNT,
            success = "解绑了身份源（{{ @linkGen.toLink(#registrationId, T(ResourceType).IDENTITY_SOURCE) }}）",
            fail = "解绑身份源（{{ @linkGen.toLink(#registrationId, T(ResourceType).IDENTITY_SOURCE) }}）失败"
    )
    @Override
    public void unbindUser(String registrationId) {
        // 1. 获取当前用户ID
        String userId = AuthUtil.getCurrentUserId();

        // 2. 删除第三方账号
        thirdAccountService.remove(Wrappers.<ThirdAccount>lambdaQuery().eq(ThirdAccount::getRegistrationId, registrationId).eq(ThirdAccount::getUserId, userId));
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
