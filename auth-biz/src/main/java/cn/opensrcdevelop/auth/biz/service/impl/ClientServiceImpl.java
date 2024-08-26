package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.component.DbRegisteredClientRepository;
import cn.opensrcdevelop.auth.biz.dto.ClientRequestDto;
import cn.opensrcdevelop.auth.biz.dto.ClientResponseDto;
import cn.opensrcdevelop.auth.biz.dto.CreateOrUpdateSecretClientResponseDto;
import cn.opensrcdevelop.auth.biz.entity.Client;
import cn.opensrcdevelop.auth.biz.entity.ResourceGroup;
import cn.opensrcdevelop.auth.biz.mapper.ClientMapper;
import cn.opensrcdevelop.auth.biz.service.ClientService;
import cn.opensrcdevelop.auth.biz.service.ResourceGroupService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
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
@Transactional
@RequiredArgsConstructor
public class ClientServiceImpl extends ServiceImpl<ClientMapper, Client> implements ClientService {

    private final DbRegisteredClientRepository registeredClientRepository;
    private final PasswordEncoder passwordEncoder;
    private final ResourceGroupService resourceGroupService;

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
    @Override
    public CreateOrUpdateSecretClientResponseDto createClient(ClientRequestDto requestDto) {
        CreateOrUpdateSecretClientResponseDto responseDto = new CreateOrUpdateSecretClientResponseDto();
        // 1. 客户端编辑
        // 1.1 生成客户端随机密钥
        String clientSecret = CommonUtil.getBase32StringKey(CLIENT_SECRETS_BYTES);
        RegisteredClient registeredClient = editInsertClient(requestDto, clientSecret);

        // 2. 存入数据库
        registeredClientRepository.save(registeredClient);
        // 2.1 更新客户端描述信息
        if (StringUtils.isNotEmpty(requestDto.getDesc())) {
            super.update(Wrappers.<Client>lambdaUpdate()
                    .set(Client::getDescription, requestDto.getDesc())
                    .eq(Client::getClientId, registeredClient.getClientId()));
        }

        // 3. 响应编辑
        responseDto.setId(registeredClient.getClientId());
        // 3.1 设置生成的明文密钥
        responseDto.setSecret(clientSecret);
        responseDto.setRedirectUri(requestDto.getRedirectUri());

        // 4. 设置资源组
        ResourceGroup resourceGroup = new ResourceGroup();
        resourceGroup.setResourceGroupId(CommonUtil.getUUIDString());
        resourceGroup.setResourceGroupName(registeredClient.getClientName());
        resourceGroup.setResourceGroupCode(registeredClient.getClientId());
        resourceGroupService.save(resourceGroup);

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
        return clientResponseDto;
    }

    /**
     * 更新客户端
     *
     * @param requestDto 请求
     */
    @Override
    public void updateClient(ClientRequestDto requestDto) {
        // 获取版本号
        Client rawClient = super.getById(requestDto.getId());
        if (Objects.isNull(rawClient)) {
            return;
        }

        // 更新客户端
        Client client = editUpdateClient(requestDto);
        client.setVersion(rawClient.getVersion());
        super.updateById(client);
    }

    /**
     * 更新客户端密钥
     *
     * @param id 客户端 ID
     */
    @Override
    public CreateOrUpdateSecretClientResponseDto updateClientSecret(String id) {
        var responseDto = new CreateOrUpdateSecretClientResponseDto();

        // 获取版本号
        Client rawClient = super.getById(id);
        if (Objects.isNull(rawClient)) {
            return responseDto;
        }

        // 更新客户端密钥
        Client updateClient = new Client();
        updateClient.setVersion(rawClient.getVersion());
        String secret = CommonUtil.getBase32StringKey(CLIENT_SECRETS_BYTES);
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
    @Override
    public void deleteClient(String clientId) {
        super.removeById(clientId);
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
        clientBuilder.clientSettings(clientSettingsBuilder.build());

        return clientBuilder.build();
    }
}
