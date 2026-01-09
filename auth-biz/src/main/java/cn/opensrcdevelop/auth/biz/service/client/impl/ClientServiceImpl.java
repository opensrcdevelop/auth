package cn.opensrcdevelop.auth.biz.service.client.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.component.authserver.DbRegisteredClientRepository;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.SystemSettingConstants;
import cn.opensrcdevelop.auth.biz.dto.client.ClientRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.ClientResponseDto;
import cn.opensrcdevelop.auth.biz.dto.client.CreateOrUpdateSecretClientResponseDto;
import cn.opensrcdevelop.auth.biz.entity.client.Client;
import cn.opensrcdevelop.auth.biz.entity.resource.group.ResourceGroup;
import cn.opensrcdevelop.auth.biz.mapper.client.ClientMapper;
import cn.opensrcdevelop.auth.biz.service.client.ClientService;
import cn.opensrcdevelop.auth.biz.service.resource.group.ResourceGroupService;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
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
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.oauth2.core.ClientAuthenticationMethod;
import org.springframework.security.oauth2.server.authorization.client.RegisteredClient;
import org.springframework.security.oauth2.server.authorization.settings.ClientSettings;
import org.springframework.security.oauth2.server.authorization.settings.ConfigurationSettingNames;
import org.springframework.security.oauth2.server.authorization.settings.TokenSettings;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ClientServiceImpl extends ServiceImpl<ClientMapper, Client> implements ClientService {

    private final DbRegisteredClientRepository registeredClientRepository;
    private final PasswordEncoder passwordEncoder;
    private final ResourceGroupService resourceGroupService;
    private final SystemSettingService systemSettingService;

    private static final Integer CLIENT_SECRETS_BYTES = 40;

    @Override
    public Client findByClientId(String clientId) {
        return super.getOne(Wrappers.<Client>lambdaQuery().eq(Client::getClientId, clientId));
    }

    /**
     * 创建客户端
     *
     * @param requestDto 请求
     * @return 响应
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.CLIENT,
            sysOperation = SysOperationType.CREATE,
            success = "创建了客户端{{ @linkGen.toLink(#clientId, T(ResourceType).CLIENT) }}）",
            fail = "创建客户端{{ #requestDto.name }}）失败"
    )
    @Transactional
    @Override
    public CreateOrUpdateSecretClientResponseDto createClient(ClientRequestDto requestDto) {
        // 1. 检查
        // 1.1 检查客户端名称是否存在
        checkClientName(requestDto, null);
        // 1.2 检查是否为控制台客户端
        checkIsConsoleClient(requestDto.getId());

        CreateOrUpdateSecretClientResponseDto responseDto = new CreateOrUpdateSecretClientResponseDto();
        // 2. 客户端编辑
        // 2.1 生成客户端随机密钥
        String clientSecret = CommonUtil.getBase32StringKey(CLIENT_SECRETS_BYTES);
        RegisteredClient registeredClient = editInsertClient(requestDto, clientSecret);

        // 3. 存入数据库
        registeredClientRepository.save(registeredClient);
        // 3.1 更新客户端描述信息
        if (StringUtils.isNotEmpty(requestDto.getDesc())) {
            super.update(Wrappers.<Client>lambdaUpdate()
                    .set(Client::getDescription, requestDto.getDesc())
                    .eq(Client::getClientId, registeredClient.getClientId()));
        }

        // 4. 响应编辑
        responseDto.setId(registeredClient.getClientId());
        // 4.1 设置生成的明文密钥
        responseDto.setSecret(clientSecret);
        responseDto.setRedirectUri(requestDto.getRedirectUri());

        // 5. 设置资源组
        ResourceGroup resourceGroup = new ResourceGroup();
        resourceGroup.setResourceGroupId(CommonUtil.getUUIDV7String());
        resourceGroup.setResourceGroupName(registeredClient.getClientName());
        resourceGroup.setResourceGroupCode(registeredClient.getClientId());
        resourceGroupService.save(resourceGroup);

        AuditContext.setSpelVariable("clientId", registeredClient.getClientId());
        return responseDto;
    }

    /**
     * 获取客户端列表
     *
     * @param keyword 关键字
     * @param page 页数
     * @param size 条数
     * @return 客户端列表
     */
    @Override
    public PageData<ClientResponseDto> list(String keyword, int page, int size) {
        // 1. 查询数据库
        List<Client> clientList;
        Page<Client> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            clientList = super.list(pageRequest, Wrappers.<Client>lambdaQuery()
                    .select(Client::getClientId, Client::getClientName, Client::getDescription)
                    .like(Client::getClientName, keyword)
                    .orderByAsc(Client::getClientName)
            );
        } else {
            clientList = super.list(pageRequest, Wrappers.<Client>lambdaQuery()
                    .select(Client::getClientId, Client::getClientName, Client::getDescription)
                    .orderByAsc(Client::getClientName)
            );
        }

        // 2. 属性编辑
        PageData<ClientResponseDto> pageData = new PageData<>();
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());

        List<ClientResponseDto> data = CommonUtil.stream(clientList).map(c -> {
            ClientResponseDto clientResponse = new ClientResponseDto();
            clientResponse.setId(c.getClientId());
            clientResponse.setName(c.getClientName());
            clientResponse.setDesc(c.getDescription());
            return clientResponse;
        }).toList();
        pageData.setList(data);
        return pageData;
    }

    /**
     * 获取客户端详情
     *
     * @param id 客户端 ID
     * @return 客户端详情
     */
    @Override
    public ClientResponseDto details(String id) {
        // 1. 查询数据库
        Client client = super.getById(id);

        // 2. 属性设置
        ClientResponseDto clientResponseDto = new ClientResponseDto();
        if (client == null) {
            return clientResponseDto;
        }

        RegisteredClient registeredClient = registeredClientRepository.toObject(client);
        clientResponseDto.setId(client.getClientId());
        clientResponseDto.setName(client.getClientName());
        clientResponseDto.setDesc(client.getDescription());
        clientResponseDto.setRedirectUri(registeredClient.getRedirectUris().iterator().next());

        // 授权类型
        List<String> grantTypes = new ArrayList<>();
        CommonUtil.stream(registeredClient.getAuthorizationGrantTypes()).forEach(g -> grantTypes.add(g.getValue()));
        clientResponseDto.setGrantTypes(grantTypes);

        // 认证方式
        List<String> authenticationMethods = new ArrayList<>();
        CommonUtil.stream(registeredClient.getClientAuthenticationMethods()).forEach(m -> authenticationMethods.add(m.getValue()));
        clientResponseDto.setAuthenticationMethods(authenticationMethods);

        // OIDC scope
        List<String> scopes = new ArrayList<>();
        CommonUtil.stream(registeredClient.getScopes()).forEach(scopes::add);
        clientResponseDto.setScopes(scopes);

        // Token 配置
        clientResponseDto.setAuthorizationCodeTimeToLive(registeredClient.getTokenSettings().getAuthorizationCodeTimeToLive().toMinutes());
        clientResponseDto.setAccessTokenTimeToLive(registeredClient.getTokenSettings().getAccessTokenTimeToLive().toMinutes());
        clientResponseDto.setRefreshTokenTimeToLive(registeredClient.getTokenSettings().getRefreshTokenTimeToLive().toMinutes());

        // PKCE
        clientResponseDto.setRequireProofKey(registeredClient.getClientSettings().isRequireProofKey());
        return clientResponseDto;
    }

    /**
     * 更新客户端
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.CLIENT,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了客户端（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).CLIENT) }}）",
            fail = "修改客户端（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).CLIENT) }}）失败"
    )
    @Transactional
    @Override
    public void updateClient(ClientRequestDto requestDto) {
        String clientId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        Client rawClient = super.getById(clientId);
        if (Objects.isNull(rawClient)) {
            return;
        }
        compareObjBuilder.id(clientId);
        compareObjBuilder.before(rawClient);

        // 2. 检查
        // 2.1 检查客户端名称是否存在
        checkClientName(requestDto, rawClient);
        // 2.2 检查是否为控制台客户端
        checkIsConsoleClient(clientId);

        // 3. 更新客户端
        Client client = editUpdateClient(requestDto);
        client.setVersion(rawClient.getVersion());
        super.updateById(client);

        compareObjBuilder.after(super.getById(clientId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 更新客户端密钥
     *
     * @param id 客户端 ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.CLIENT,
            sysOperation = SysOperationType.UPDATE,
            success = "更新了客户端（{{ @linkGen.toLink(#id, T(ResourceType).CLIENT) }}）的密钥",
            fail = "更新客户端（{{ @linkGen.toLink(#id, T(ResourceType).CLIENT) }}）的密钥失败"
    )
    @Transactional
    @Override
    public CreateOrUpdateSecretClientResponseDto updateClientSecret(String id) {
        // 检查是否为控制台客户端
        checkIsConsoleClient(id);

        var responseDto = new CreateOrUpdateSecretClientResponseDto();

        // 获取版本号
        Client rawClient = super.getById(id);
        if (Objects.isNull(rawClient)) {
            return responseDto;
        }

        // 更新客户端密钥
        String secret = CommonUtil.getBase32StringKey(CLIENT_SECRETS_BYTES);
        Client updateClient = new Client();
        updateClient.setVersion(rawClient.getVersion());
        updateClient.setClientId(rawClient.getClientId());
        updateClient.setClientSecret(passwordEncoder.encode(secret));
        super.updateById(updateClient);

        responseDto.setSecret(secret);
        return responseDto;
    }

    /**
     * 删除客户端
     *
     * @param clientId 客户端ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.CLIENT,
            sysOperation = SysOperationType.DELETE,
            success = "删除了客户端（{{ @linkGen.toLink(#clientId, T(ResourceType).CLIENT) }}）",
            fail = "删除客户端（{{ @linkGen.toLink(#clientId, T(ResourceType).CLIENT) }}）失败"
    )
    @Transactional
    @Override
    public void deleteClient(String clientId) {
        // 1. 检查是否为控制台客户端
        checkIsConsoleClient(clientId);

        // 2. 数据库操作
        super.removeById(clientId);
    }

    /**
     * 轮换控制台客户端密钥
     *
     * @return 轮换后的密钥
     */
    @Override
    public String rotateConsoleClientSecret() {
        String consoleClientId = systemSettingService.getSystemSetting(SystemSettingConstants.CONSOLE_CLIENT_ID, String.class);

        // 获取版本号
        Integer version = super.getById(consoleClientId).getVersion();

        // 更新客户端密钥
        String secret = CommonUtil.getBase32StringKey(CLIENT_SECRETS_BYTES);
        Client updateClient = new Client();
        updateClient.setVersion(version == null ? 1 : version);
        updateClient.setClientId(consoleClientId);
        updateClient.setClientSecret(passwordEncoder.encode(secret));
        super.updateById(updateClient);

        return secret;
    }

    private Client editUpdateClient(ClientRequestDto requestDto) {
        Client updateClient = new Client();
        updateClient.setClientId(requestDto.getId());

        // 1. 普通属性设置
        CommonUtil.callSetWithCheck(StringUtils::isNotBlank, updateClient::setClientName, requestDto::getName);
        CommonUtil.callSetWithCheck(StringUtils::isNotBlank, updateClient::setRedirectUris, requestDto::getRedirectUri);
        updateClient.setDescription(requestDto.getDesc());

        if (CollectionUtils.isNotEmpty(requestDto.getGrantTypes())) {
            updateClient.setAuthorizationGrantTypes(String.join(CommonConstants.COMMA, requestDto.getGrantTypes()));
        }

        if (CollectionUtils.isNotEmpty(requestDto.getAuthenticationMethods())) {
            updateClient.setClientAuthenticationMethods(String.join(CommonConstants.COMMA, requestDto.getAuthenticationMethods()));
        }

        if (Objects.nonNull(requestDto.getScopes())) {
            if (!requestDto.getScopes().isEmpty()) {
                updateClient.setScopes(String.join(CommonConstants.COMMA, requestDto.getScopes()));
            } else {
                updateClient.setScopes("");
            }
        }

        // 2. Json 属性设置
        // 2.1 获取更新前 Client
        RegisteredClient beforeUpdateClient = registeredClientRepository.findByClientId(requestDto.getId());
        if (beforeUpdateClient != null) {
            Map<String, Object> tokenSettings = new HashMap<>(beforeUpdateClient.getTokenSettings().getSettings());
            Map<String, Object> clientSettings = new HashMap<>(beforeUpdateClient.getClientSettings().getSettings());

            if (Objects.nonNull(requestDto.getAuthorizationCodeTimeToLive())) {
                tokenSettings.put(ConfigurationSettingNames.Token.AUTHORIZATION_CODE_TIME_TO_LIVE, Duration.ofMinutes(requestDto.getAuthorizationCodeTimeToLive()));
            }

            if (Objects.nonNull(requestDto.getAccessTokenTimeToLive())) {
                tokenSettings.put(ConfigurationSettingNames.Token.ACCESS_TOKEN_TIME_TO_LIVE, Duration.ofMinutes(requestDto.getAccessTokenTimeToLive()));
            }

            if (Objects.nonNull(requestDto.getRefreshTokenTimeToLive())) {
                tokenSettings.put(ConfigurationSettingNames.Token.REFRESH_TOKEN_TIME_TO_LIVE, Duration.ofMinutes(requestDto.getRefreshTokenTimeToLive()));
            }

            if (Objects.nonNull(requestDto.getRequireAuthorizationConsent())) {
                clientSettings.put(ConfigurationSettingNames.Client.REQUIRE_AUTHORIZATION_CONSENT, requestDto.getRequireAuthorizationConsent());
            }

            if (Objects.nonNull(requestDto.getRequireProofKey())) {
                clientSettings.put(ConfigurationSettingNames.Client.REQUIRE_PROOF_KEY, requestDto.getRequireProofKey());
            }

            // 2.1.1 Token 属性设置
            updateClient.setTokenSettings(AuthUtil.writeMap(tokenSettings));
            // 2.1.2 Client 属性设置
            updateClient.setClientSettings(AuthUtil.writeMap(clientSettings));
        }

        return updateClient;
    }

    private RegisteredClient editInsertClient(ClientRequestDto requestDto, String clientSecret) {
        RegisteredClient.Builder clientBuilder = RegisteredClient.withId(UUID.randomUUID().toString());
        TokenSettings.Builder tokenSettingsBuilder = TokenSettings.builder();
        ClientSettings.Builder clientSettingsBuilder = ClientSettings.builder();

        clientBuilder.clientName(requestDto.getName());
        clientBuilder.clientId(UUID.randomUUID().toString());
        clientBuilder.clientIdIssuedAt(Instant.now());

        // 客户端密钥加密
        clientBuilder.clientSecret(passwordEncoder.encode(clientSecret));

        clientBuilder.redirectUri(requestDto.getRedirectUri());

        if (CollectionUtils.isNotEmpty(requestDto.getScopes())) {
            clientBuilder.scopes(x -> x.addAll(requestDto.getScopes()));
        }

        var grantTypes = requestDto.getGrantTypes().stream().distinct().map(AuthorizationGrantType::new).collect(Collectors.toSet());
        clientBuilder.authorizationGrantTypes(x -> x.addAll(grantTypes));

        var methods = requestDto.getAuthenticationMethods().stream().distinct().map(ClientAuthenticationMethod::new).collect(Collectors.toSet());
        clientBuilder.clientAuthenticationMethods(x -> x.addAll(methods));

        tokenSettingsBuilder.authorizationCodeTimeToLive(Duration.ofMinutes(requestDto.getAuthorizationCodeTimeToLive()));
        tokenSettingsBuilder.accessTokenTimeToLive(Duration.ofMinutes(requestDto.getAccessTokenTimeToLive()));
        tokenSettingsBuilder.refreshTokenTimeToLive(Duration.ofMinutes(requestDto.getRefreshTokenTimeToLive()));
        clientBuilder.tokenSettings(tokenSettingsBuilder.build());

        clientSettingsBuilder.requireAuthorizationConsent(!Objects.isNull(requestDto.getRequireAuthorizationConsent()) && requestDto.getRequireAuthorizationConsent());
        clientSettingsBuilder.requireProofKey(!Objects.isNull(requestDto.getRequireProofKey()) && requestDto.getRequireProofKey());
        clientBuilder.clientSettings(clientSettingsBuilder.build());

        return clientBuilder.build();
    }

    private void checkClientName(ClientRequestDto requestDto, Client rawClient) {
        if (Objects.nonNull(rawClient) && StringUtils.equals(requestDto.getName(), rawClient.getClientName())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<Client>lambdaQuery().eq(Client::getClientName, requestDto.getName())))) {
            throw new BizException(MessageConstants.CLIENT_MSG_1000, requestDto.getName());
        }
    }

    private void checkIsConsoleClient(String clientId) {
        String consoleClientId =  systemSettingService.getSystemSetting(SystemSettingConstants.CONSOLE_CLIENT_ID, String.class);
        if (StringUtils.equals(consoleClientId, clientId)) {
            throw new BizException(MessageConstants.CLIENT_MSG_1001, clientId);
        }
    }
}
