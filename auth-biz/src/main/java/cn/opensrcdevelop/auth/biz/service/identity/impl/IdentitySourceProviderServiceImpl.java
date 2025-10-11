package cn.opensrcdevelop.auth.biz.service.identity.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceProviderRequestDto;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceProviderResponseDto;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceRegistrationResponseDto;
import cn.opensrcdevelop.auth.biz.dto.identity.RequestConfigRequestDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceProvider;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.mapper.identity.IdentitySourceProviderMapper;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceProviderService;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class IdentitySourceProviderServiceImpl extends ServiceImpl<IdentitySourceProviderMapper, IdentitySourceProvider> implements IdentitySourceProviderService {

    private final IdentitySourceRegistrationService identitySourceRegistrationService;

    /**
     * 创建身份源提供商
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.IDENTITY_SOURCE_PROVIDER,
            sysOperation = SysOperationType.CREATE,
            success = "创建了身份源提供商（{{ #providerId }}）",
            fail = "创建身份源提供商（{{ #requestDto.name }}）失败"
    )
    @Transactional
    @Override
    public void createIdentitySourceProvider(IdentitySourceProviderRequestDto requestDto) {
        // 1. 检查身份源提供商标识是否存在
        checkProviderCode(requestDto, null);

        // 2. 检查请求配置
        checkRequestCfg(requestDto);

        // 3. 属性编辑
        String providerId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("providerId", providerId);

        IdentitySourceProvider identitySourceProvider = new IdentitySourceProvider();
        identitySourceProvider.setProviderId(providerId);
        identitySourceProvider.setProviderName(requestDto.getName());
        identitySourceProvider.setProviderCode(requestDto.getCode());
        identitySourceProvider.setProviderLogo(requestDto.getLogo());
        identitySourceProvider.setProviderDesc(requestDto.getDesc());
        identitySourceProvider.setAuthorizationUri(requestDto.getAuthorizationUri());
        identitySourceProvider.setTokenUri(requestDto.getTokenUri());
        identitySourceProvider.setUserInfoUris(CommonUtil.stream(requestDto.getUserInfoUris()).collect(Collectors.joining(CommonConstants.COMMA)));
        identitySourceProvider.setScopes(CommonUtil.stream(requestDto.getScopes()).collect(Collectors.joining(CommonConstants.COMMA)));
        identitySourceProvider.setUsernameAttribute(requestDto.getUsernameAttribute());
        identitySourceProvider.setUniqueIdAttribute(requestDto.getUniqueIdAttribute());
        identitySourceProvider.setUserMatchAttribute(requestDto.getUserMatchAttribute());
        identitySourceProvider.setJwkSetUri(requestDto.getJwkSetUri());
        identitySourceProvider.setUserInfoAuthenticationMethod(requestDto.getUserInfoAuthenticationMethod());

        // 自定义授权请求
        identitySourceProvider.setEnableCustomAuthzReq(requestDto.getEnableCustomAuthzReq());
        if (Boolean.TRUE.equals(requestDto.getEnableCustomAuthzReq())) {
            identitySourceProvider.setAuthzReqCfg(CommonUtil.formatJson(requestDto.getAuthzReqCfg()));
        }

        // 自定义令牌请求
        identitySourceProvider.setEnableCustomTokenReq(requestDto.getEnableCustomTokenReq());
        if (Boolean.TRUE.equals(requestDto.getEnableCustomTokenReq())) {
            identitySourceProvider.setTokenReqCfg(CommonUtil.formatJson(requestDto.getTokenReqCfg()));
        }

        // 自定义用户信息请求
        identitySourceProvider.setEnableCustomUserInfoReq(requestDto.getEnableCustomUserInfoReq());
        if (Boolean.TRUE.equals(requestDto.getEnableCustomUserInfoReq())) {
            identitySourceProvider.setUserInfoReqCfg(CommonUtil.formatJson(requestDto.getUserInfoReqCfg()));
        }

        // 4. 数据库操作
        super.save(identitySourceProvider);
    }

    /**
     * 更新身份源提供商
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.IDENTITY_SOURCE_PROVIDER,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了身份源提供商（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).IDENTITY_SOURCE_PROVIDER) }}）",
            fail = "修改身份源提供商（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).IDENTITY_SOURCE_PROVIDER) }}）失败"
    )
    @Transactional
    @Override
    public void updateIdentitySourceProvider(IdentitySourceProviderRequestDto requestDto) {
        String providerId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 检查请求配置
        checkRequestCfg(requestDto);

        // 2. 获取版本号
        var rawProvider = super.getById(providerId);
        if (Objects.isNull(rawProvider)) {
            return;
        }
        compareObjBuilder.id(providerId);
        compareObjBuilder.before(rawProvider);

        // 3. 检查身份源提供商标识是否存在
        checkProviderCode(requestDto, rawProvider);

        // 4. 属性编辑
        IdentitySourceProvider updateProvider = new IdentitySourceProvider();
        updateProvider.setProviderId(requestDto.getId());
        updateProvider.setProviderName(requestDto.getName());
        updateProvider.setProviderCode(requestDto.getCode());
        updateProvider.setProviderLogo(requestDto.getLogo());
        updateProvider.setProviderDesc(requestDto.getDesc());
        updateProvider.setAuthorizationUri(requestDto.getAuthorizationUri());
        updateProvider.setTokenUri(requestDto.getTokenUri());

        if (CollectionUtils.isNotEmpty(requestDto.getUserInfoUris())) {
            updateProvider.setUserInfoUris(CommonUtil.stream(requestDto.getUserInfoUris()).collect(Collectors.joining(CommonConstants.COMMA)));
        }

        updateProvider.setScopes(CommonUtil.stream(requestDto.getScopes()).collect(Collectors.joining(CommonConstants.COMMA)));
        updateProvider.setUsernameAttribute(requestDto.getUsernameAttribute());
        updateProvider.setUserMatchAttribute(requestDto.getUserMatchAttribute());
        updateProvider.setUniqueIdAttribute(requestDto.getUniqueIdAttribute());
        updateProvider.setJwkSetUri(requestDto.getJwkSetUri());
        updateProvider.setUserInfoAuthenticationMethod(requestDto.getUserInfoAuthenticationMethod());

        // 自定义授权请求
        if (Objects.nonNull(requestDto.getEnableCustomAuthzReq())) {
            updateProvider.setEnableCustomAuthzReq(requestDto.getEnableCustomAuthzReq());
            if (Boolean.TRUE.equals(requestDto.getEnableCustomAuthzReq())) {
                updateProvider.setAuthzReqCfg(CommonUtil.formatJson(requestDto.getAuthzReqCfg()));
            }
        }

        // 自定义令牌请求
        if (Objects.nonNull(requestDto.getEnableCustomTokenReq())) {
            updateProvider.setEnableCustomTokenReq(requestDto.getEnableCustomTokenReq());
            if (Boolean.TRUE.equals(requestDto.getEnableCustomTokenReq())) {
                updateProvider.setTokenReqCfg(CommonUtil.formatJson(requestDto.getTokenReqCfg()));
            }
        }

        // 自定义用户信息请求
        if (Objects.nonNull(requestDto.getEnableCustomUserInfoReq())) {
            updateProvider.setEnableCustomUserInfoReq(requestDto.getEnableCustomUserInfoReq());
            if (Boolean.TRUE.equals(requestDto.getEnableCustomUserInfoReq())) {
                updateProvider.setUserInfoReqCfg(CommonUtil.formatJson(requestDto.getUserInfoReqCfg()));
            }
        }

        updateProvider.setVersion(rawProvider.getVersion());

        // 5. 数据库操作
        super.updateById(updateProvider);

        compareObjBuilder.after(super.getById(providerId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 删除身份源提供商
     *
     * @param providerId 身份源提供商ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.IDENTITY_SOURCE_PROVIDER,
            sysOperation = SysOperationType.DELETE,
            success = "删除了身份源提供商（{{ @linkGen.toLink(#providerId, T(ResourceType).IDENTITY_SOURCE_PROVIDER) }}）",
            fail = "删除身份源提供商（{{ @linkGen.toLink(#providerId, T(ResourceType).IDENTITY_SOURCE_PROVIDER) }}）失败"
    )
    @Transactional
    @Override
    public void removeIdentitySourceProvider(String providerId) {
        // 1. 删除身份源提供商
        super.removeById(providerId);

        // 2. 获取关联的身份源注册商 ID
        var registrationIds = CommonUtil.stream(
                identitySourceRegistrationService.list(Wrappers.<IdentitySourceRegistration>lambdaQuery()
                        .select(IdentitySourceRegistration::getRegistrationId)
                        .eq(IdentitySourceRegistration::getProviderId, providerId)))
                .map(IdentitySourceRegistration::getRegistrationId).toList();

        // 3. 删除关联的身份源注册商
        if (CollectionUtils.isNotEmpty(registrationIds)) {
            identitySourceRegistrationService.removeBatchByIds(registrationIds);
        }

        // 4. 删除关联的第三方账号
        if (CollectionUtils.isNotEmpty(registrationIds)) {
            identitySourceRegistrationService.removeIdentitySourceRegistrations(registrationIds);
        }
    }

    /**
     * 获取身份源提供商列表
     *
     * @param page 页数
     * @param size 条数
     * @param keyword 身份源提供商名称 / 标识检索关键字
     * @return 身份源提供商列表
     */
    @Override
    public PageData<IdentitySourceProviderResponseDto> list(int page, int size, String keyword) {
        // 1. 数据库操作
        Page<IdentitySourceProvider> pageRequest = new Page<>(page, size);
        List<IdentitySourceProvider> providers;
        if (StringUtils.isNotEmpty(keyword)) {
            providers = super.list(pageRequest,
                    Wrappers.<IdentitySourceProvider>lambdaQuery()
                            .select(IdentitySourceProvider::getProviderId, IdentitySourceProvider::getProviderName, IdentitySourceProvider::getProviderCode, IdentitySourceProvider::getProviderLogo, IdentitySourceProvider::getProviderDesc)
                            .like(IdentitySourceProvider::getProviderName, keyword).or().like(IdentitySourceProvider::getProviderCode, keyword)
                            .orderByAsc(IdentitySourceProvider::getProviderCode));
        } else {
            providers = super.list(pageRequest,
                    Wrappers.<IdentitySourceProvider>lambdaQuery()
                            .select(IdentitySourceProvider::getProviderId, IdentitySourceProvider::getProviderName, IdentitySourceProvider::getProviderCode, IdentitySourceProvider::getProviderLogo, IdentitySourceProvider::getProviderDesc)
                            .orderByAsc(IdentitySourceProvider::getProviderCode));
        }

        // 2. 属性设置
        PageData<IdentitySourceProviderResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var records = CommonUtil.stream(providers).map(provider ->
            IdentitySourceProviderResponseDto.builder()
                    .id(provider.getProviderId())
                    .name(provider.getProviderName())
                    .code(provider.getProviderCode())
                    .logo(provider.getProviderLogo())
                    .desc(provider.getProviderDesc()).build()).toList();
        pageData.setList(records);
        return pageData;
    }

    /**
     * 获取身份源提供商详情
     *
     * @param providerId 身份源提供商ID
     * @return 身份源提供商详情
     */
    @Override
    public IdentitySourceProviderResponseDto detail(String providerId) {
        // 1. 数据库操作
        IdentitySourceProvider provider = super.getById(providerId);
        if (Objects.isNull(provider)) {
            return IdentitySourceProviderResponseDto.builder().build();
        }

        // 2. 属性设置
        var builder = IdentitySourceProviderResponseDto.builder();
        builder
                .id(provider.getProviderId())
                .name(provider.getProviderName())
                .code(provider.getProviderCode())
                .logo(provider.getProviderLogo())
                .desc(provider.getProviderDesc())
                .authorizationUri(provider.getAuthorizationUri())
                .tokenUri(provider.getTokenUri())
                .userInfoUris(Arrays.asList(provider.getUserInfoUris().split(CommonConstants.COMMA)))
                .scopes(Arrays.asList(provider.getScopes().split(CommonConstants.COMMA)))
                .usernameAttribute(provider.getUsernameAttribute())
                .uniqueIdAttribute(provider.getUniqueIdAttribute())
                .userMatchAttribute(provider.getUserMatchAttribute())
                .jwkSetUri(provider.getJwkSetUri())
                .userInfoAuthenticationMethod(provider.getUserInfoAuthenticationMethod())
                .enableCustomAuthzReq(provider.getEnableCustomAuthzReq())
                .authzReqCfg(provider.getAuthzReqCfg())
                .enableCustomTokenReq(provider.getEnableCustomTokenReq())
                .tokenReqCfg(provider.getTokenReqCfg())
                .enableCustomUserInfoReq(provider.getEnableCustomUserInfoReq())
                .userInfoReqCfg(provider.getUserInfoReqCfg());

        return builder.build();
    }

    /**
     * 获取关联的身份源
     *
     * @param providerId 身份源提供商ID
     * @return 身份源列表
     */
    @Override
    public List<IdentitySourceRegistrationResponseDto> registrations(String providerId) {
        // 1. 数据库操作
        var records = identitySourceRegistrationService.list(Wrappers.<IdentitySourceRegistration>lambdaQuery().eq(IdentitySourceRegistration::getProviderId, providerId));

        // 2. 属性编辑
        return CommonUtil.stream(records).map(registration ->
            IdentitySourceRegistrationResponseDto.builder()
                    .id(registration.getRegistrationId())
                    .code(registration.getRegistrationCode())
                    .name(registration.getRegistrationName())
                    .enabled(registration.getEnabled()).build()
        ).toList();
    }

    private void checkProviderCode(IdentitySourceProviderRequestDto requestDto, IdentitySourceProvider rawProvider) {
        if (Objects.nonNull(rawProvider) && StringUtils.equals(requestDto.getCode(), rawProvider.getProviderCode())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<IdentitySourceProvider>lambdaQuery().eq(IdentitySourceProvider::getProviderCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.IDENTITY_PROVIDER_MSG_1000, requestDto.getCode());
        }
    }

    public void checkRequestCfg(IdentitySourceProviderRequestDto requestDto) {
        // 授权请求
        if (Boolean.TRUE.equals(requestDto.getEnableCustomAuthzReq())) {
            CommonUtil.validateBean(requestDto.getAuthzReqCfg(), RequestConfigRequestDto.AuthzReq.class);
        }

        // 令牌请求
        if (Boolean.TRUE.equals(requestDto.getEnableCustomTokenReq())) {
            String requestMethod = requestDto.getTokenReqCfg().getMethod();
            if (HttpMethod.GET.matches(requestMethod)) {
                CommonUtil.validateBean(requestDto.getTokenReqCfg(), RequestConfigRequestDto.TokenReq.class, RequestConfigRequestDto.GetReq.class);
            }

            if (HttpMethod.POST.matches(requestMethod)) {
                CommonUtil.validateBean(requestDto.getTokenReqCfg(), RequestConfigRequestDto.TokenReq.class, RequestConfigRequestDto.PostReq.class);
            }
        }

        // 用户信息请求
        if (Boolean.TRUE.equals(requestDto.getEnableCustomUserInfoReq())) {
            requestDto.getUserInfoReqCfg().values().forEach(userIndoReqCfg -> {
                String requestMethod = userIndoReqCfg.getMethod();
                if (HttpMethod.GET.matches(requestMethod)) {
                    CommonUtil.validateBean(requestDto.getUserInfoReqCfg(), RequestConfigRequestDto.UserInfoReq.class, RequestConfigRequestDto.GetReq.class);
                }

                if (HttpMethod.POST.matches(requestMethod)) {
                    CommonUtil.validateBean(requestDto.getUserInfoReqCfg(), RequestConfigRequestDto.UserInfoReq.class, RequestConfigRequestDto.PostReq.class);
                }
            });
        }

    }
}
