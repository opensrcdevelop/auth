package cn.opensrcdevelop.auth.biz.component;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.entity.Authorization;
import cn.opensrcdevelop.auth.biz.entity.LoginLog;
import cn.opensrcdevelop.auth.biz.event.ClearExpiredTokensEvent;
import cn.opensrcdevelop.auth.biz.repository.AuthorizationRepository;
import cn.opensrcdevelop.auth.biz.service.LoginLogService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import jakarta.servlet.http.HttpSession;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.security.oauth2.core.*;
import org.springframework.security.oauth2.core.endpoint.OAuth2ParameterNames;
import org.springframework.security.oauth2.core.oidc.OidcIdToken;
import org.springframework.security.oauth2.core.oidc.endpoint.OidcParameterNames;
import org.springframework.security.oauth2.server.authorization.OAuth2Authorization;
import org.springframework.security.oauth2.server.authorization.OAuth2AuthorizationCode;
import org.springframework.security.oauth2.server.authorization.OAuth2AuthorizationService;
import org.springframework.security.oauth2.server.authorization.OAuth2TokenType;
import org.springframework.security.oauth2.server.authorization.client.RegisteredClient;
import org.springframework.security.oauth2.server.authorization.client.RegisteredClientRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

import java.lang.reflect.Field;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Collectors;

@Component
@Slf4j
public class DbOAuth2AuthorizationService implements OAuth2AuthorizationService {

    private static final String REFRESH_TOKEN_BLACK_LIST_PREFIX = "refresh_token_black_list:";

    private final AuthorizationRepository authorizationRepository;
    private final RegisteredClientRepository registeredClientRepository;
    private final LoginLogService loginLogService;

    public DbOAuth2AuthorizationService(AuthorizationRepository authorizationRepository, RegisteredClientRepository registeredClientRepository, LoginLogService loginLogService) {
        Assert.notNull(authorizationRepository, "authorizationRepository cannot be null");
        Assert.notNull(registeredClientRepository, "registeredClientRepository cannot be null");
        Assert.notNull(loginLogService, "loginLogService cannot be null");
        this.authorizationRepository = authorizationRepository;
        this.registeredClientRepository = registeredClientRepository;
        this.loginLogService = loginLogService;
    }

    @Transactional
    @Override
    public void save(OAuth2Authorization authorization) {
        Assert.notNull(authorization, "authorization cannot be null");
        // 1. 检查 RefreshToken 是否在黑名单中
        if (Objects.nonNull(authorization.getRefreshToken()) &&
                (Objects.nonNull(RedisUtil.get(REFRESH_TOKEN_BLACK_LIST_PREFIX + authorization.getRefreshToken().getToken().getTokenValue(), String.class)))) {
            throw new OAuth2AuthenticationException("invalid refresh_token");
        }

        // 2. 避免 RefreshToken 和 AuthorizationCode 重复存储
        editOAuth2Authorization(authorization);

        // 3. 数据库操作
        Authorization entity = toEntity(authorization);
        this.authorizationRepository.save(entity);

        // 4. 清除过期的 Token
        SpringContextUtil.publishEvent(new ClearExpiredTokensEvent(authorization.getRegisteredClientId()));

        // 5. 更新登录日志的客户端ID
        loginLogService.update(Wrappers.<LoginLog>lambdaUpdate().set(LoginLog::getClientId, authorization.getRegisteredClientId()).isNull(LoginLog::getClientId).eq(LoginLog::getLoginId, entity.getLoginId()));
    }

    @Transactional
    @Override
    public void remove(OAuth2Authorization authorization) {
        Assert.notNull(authorization, "authorization cannot be null");
        authorizationRepository.deleteById(Long.valueOf(authorization.getId()));
    }

    @Override
    public OAuth2Authorization findById(String id) {
        Assert.hasText(id, "id cannot be empty");
        return Optional.ofNullable(authorizationRepository.findById(Long.valueOf(id))).map(this::toObject).orElse(null);
    }

    @Override
    public OAuth2Authorization findByToken(String token, OAuth2TokenType tokenType) {
        Assert.hasText(token, "token cannot be empty");

        Authorization result;
        if (tokenType == null) {
            result = this.authorizationRepository.findByStateOrAuthorizationCodeValueOrAccessTokenValueOrRefreshTokenValueOrOidcIdTokenValueOrUserCodeValueOrDeviceCodeValue(token);
        } else if (OAuth2ParameterNames.STATE.equals(tokenType.getValue())) {
            result = this.authorizationRepository.findByState(token);
        } else if (OAuth2ParameterNames.CODE.equals(tokenType.getValue())) {
            result = this.authorizationRepository.findByAuthorizationCodeValue(token);
        } else if (OAuth2ParameterNames.ACCESS_TOKEN.equals(tokenType.getValue())) {
            result = this.authorizationRepository.findByAccessTokenValue(token);
        } else if (OAuth2ParameterNames.REFRESH_TOKEN.equals(tokenType.getValue())) {
            result = this.authorizationRepository.findByRefreshTokenValue(token);
        } else if (OidcParameterNames.ID_TOKEN.equals(tokenType.getValue())) {
            result = this.authorizationRepository.findByOidcIdTokenValue(token);
        } else if (OAuth2ParameterNames.USER_CODE.equals(tokenType.getValue())) {
            result = this.authorizationRepository.findByUserCodeValue(token);
        } else if (OAuth2ParameterNames.DEVICE_CODE.equals(tokenType.getValue())) {
            result = this.authorizationRepository.findByDeviceCodeValue(token);
        } else {
            result = null;
        }

        return Optional.ofNullable(result).map(this::toObject).orElse(null);
    }

    public List<OAuth2Authorization> findByClientId(String clientId) {
        return CommonUtil.stream(authorizationRepository.getAuthorizationsByClientId(clientId)).map(this::toObject).collect(Collectors.toCollection(ArrayList::new));
    }

    @Transactional
    public void removeByIds(List<Long> ids) {
        authorizationRepository.deleteByIds(ids);
    }

    @Transactional
    public void removeUserTokens(String principalname) {
        // 1. 查询是否存在 RefrshToken
        List<Authorization> authorizations = authorizationRepository.findRefreshTokensByPrincipalName(principalname);
        CommonUtil.stream(authorizations).filter(authorization -> StringUtils.hasText(authorization.getRefreshTokenValue())).forEach(authorization ->
            // 2. 将 RefreshToken 加入黑名单，避免使用该 RereshToken
            RedisUtil.set(REFRESH_TOKEN_BLACK_LIST_PREFIX + authorization.getRefreshTokenValue(),
                    "",
                    Duration.between(authorization.getRefreshTokenIssuedAt(), authorization.getRefreshTokenExpiresAt()).getSeconds(), TimeUnit.SECONDS));

        // 3. 数据库操作
        authorizationRepository.deleteUserTokens(principalname);
    }

    @Transactional
    public void removeByLoginId(String loginId) {
        // 1. 查询是否存在 RefrshToken
        List<Authorization> authorizations = authorizationRepository.findRefreshTokensByLoginId(loginId);
        CommonUtil.stream(authorizations).filter(authorization -> StringUtils.hasText(authorization.getRefreshTokenValue())).forEach(authorization ->
            // 2. 将 RefreshToken 加入黑名单，避免使用该 RereshToken
            RedisUtil.set(REFRESH_TOKEN_BLACK_LIST_PREFIX + authorization.getRefreshTokenValue(),
                    "",
                    Duration.between(authorization.getRefreshTokenIssuedAt(), authorization.getRefreshTokenExpiresAt()).getSeconds(), TimeUnit.SECONDS));

        // 3. 删除关联的 session
        LoginLog loginLog = loginLogService.getById(loginId);
        if (Objects.nonNull(loginLog) && StringUtils.hasText(loginLog.getSessionId())) {
            WebUtil.removeSession(loginLog.getSessionId());
        }

        // 4. 数据库操作
        authorizationRepository.deleteByLoginId(loginId);
    }

    private OAuth2Authorization toObject(Authorization entity) {
        RegisteredClient registeredClient = this.registeredClientRepository.findByClientId(entity.getRegisteredClientId());
        if (registeredClient == null) {
            throw new DataRetrievalFailureException(
                    "The RegisteredClient with id '" + entity.getRegisteredClientId() + "' was not found in the RegisteredClientRepository.");
        }

        OAuth2Authorization.Builder builder = OAuth2Authorization.withRegisteredClient(registeredClient)
                .id(String.valueOf(entity.getId()))
                .principalName(entity.getPrincipalName())
                .authorizationGrantType(resolveAuthorizationGrantType(entity.getAuthorizationGrantType()))
                .authorizedScopes(StringUtils.commaDelimitedListToSet(entity.getAuthorizedScopes()))
                .attributes(attributes -> {
                    if (StringUtils.hasText(entity.getLoginId())) {
                        // 保存登录ID
                        attributes.put(AuthConstants.SESSION_LOGIN_ID, entity.getLoginId());
                    }
                    attributes.putAll(AuthUtil.parseMap(entity.getAttributes()));
                });
        if (entity.getState() != null) {
            builder.attribute(OAuth2ParameterNames.STATE, entity.getState());
        }

        if (entity.getAuthorizationCodeValue() != null) {
            OAuth2AuthorizationCode authorizationCode = new OAuth2AuthorizationCode(
                    entity.getAuthorizationCodeValue(),
                    entity.getAuthorizationCodeIssuedAt(),
                    entity.getAuthorizationCodeExpiresAt());
            builder.token(authorizationCode, metadata -> metadata.putAll(AuthUtil.parseMap(entity.getAuthorizationCodeMetadata())));
        }

        if (entity.getAccessTokenValue() != null) {
            OAuth2AccessToken accessToken = new OAuth2AccessToken(
                    OAuth2AccessToken.TokenType.BEARER,
                    entity.getAccessTokenValue(),
                    entity.getAccessTokenIssuedAt(),
                    entity.getAccessTokenExpiresAt(),
                    StringUtils.commaDelimitedListToSet(entity.getAccessTokenScopes()));
            builder.token(accessToken, metadata -> metadata.putAll(AuthUtil.parseMap(entity.getAccessTokenMetadata())));
        }

        if (entity.getRefreshTokenValue() != null) {
            OAuth2RefreshToken refreshToken = new OAuth2RefreshToken(
                    entity.getRefreshTokenValue(),
                    entity.getRefreshTokenIssuedAt(),
                    entity.getRefreshTokenExpiresAt());
            builder.token(refreshToken, metadata -> metadata.putAll(AuthUtil.parseMap(entity.getRefreshTokenMetadata())));
        }

        if (entity.getOidcIdTokenValue() != null) {
            OidcIdToken idToken = new OidcIdToken(
                    entity.getOidcIdTokenValue(),
                    entity.getOidcIdTokenIssuedAt(),
                    entity.getOidcIdTokenExpiresAt(),
                    AuthUtil.parseMap(entity.getOidcIdTokenClaims()));
            builder.token(idToken, metadata -> metadata.putAll(AuthUtil.parseMap(entity.getOidcIdTokenMetadata())));
        }

        if (entity.getUserCodeValue() != null) {
            OAuth2UserCode userCode = new OAuth2UserCode(
                    entity.getUserCodeValue(),
                    entity.getUserCodeIssuedAt(),
                    entity.getUserCodeExpiresAt());
            builder.token(userCode, metadata -> metadata.putAll(AuthUtil.parseMap(entity.getUserCodeMetadata())));
        }

        if (entity.getDeviceCodeValue() != null) {
            OAuth2DeviceCode deviceCode = new OAuth2DeviceCode(
                    entity.getDeviceCodeValue(),
                    entity.getDeviceCodeIssuedAt(),
                    entity.getDeviceCodeExpiresAt());
            builder.token(deviceCode, metadata -> metadata.putAll(AuthUtil.parseMap(entity.getDeviceCodeMetadata())));
        }

        return builder.build();
    }

    private Authorization toEntity(OAuth2Authorization authorization) {
        Authorization entity = new Authorization();
        entity.setRegisteredClientId(authorization.getRegisteredClientId());
        entity.setPrincipalName(authorization.getPrincipalName());
        entity.setAuthorizationGrantType(authorization.getAuthorizationGrantType().getValue());
        entity.setAuthorizedScopes(StringUtils.collectionToDelimitedString(authorization.getAuthorizedScopes(), ","));
        entity.setState(authorization.getAttribute(OAuth2ParameterNames.STATE));

        OAuth2Authorization.Token<OAuth2AuthorizationCode> authorizationCode =
                authorization.getToken(OAuth2AuthorizationCode.class);
        setTokenValues(
                authorizationCode,
                entity::setAuthorizationCodeValue,
                entity::setAuthorizationCodeIssuedAt,
                entity::setAuthorizationCodeExpiresAt,
                entity::setAuthorizationCodeMetadata
        );

        OAuth2Authorization.Token<OAuth2AccessToken> accessToken =
                authorization.getToken(OAuth2AccessToken.class);
        setTokenValues(
                accessToken,
                entity::setAccessTokenValue,
                entity::setAccessTokenIssuedAt,
                entity::setAccessTokenExpiresAt,
                entity::setAccessTokenMetadata
        );
        if (accessToken != null && accessToken.getToken().getScopes() != null) {
            entity.setAccessTokenScopes(StringUtils.collectionToDelimitedString(accessToken.getToken().getScopes(), ","));
        }

        OAuth2Authorization.Token<OAuth2RefreshToken> refreshToken =
                authorization.getToken(OAuth2RefreshToken.class);
        setTokenValues(
                refreshToken,
                entity::setRefreshTokenValue,
                entity::setRefreshTokenIssuedAt,
                entity::setRefreshTokenExpiresAt,
                entity::setRefreshTokenMetadata
        );

        OAuth2Authorization.Token<OidcIdToken> oidcIdToken =
                authorization.getToken(OidcIdToken.class);
        setTokenValues(
                oidcIdToken,
                entity::setOidcIdTokenValue,
                entity::setOidcIdTokenIssuedAt,
                entity::setOidcIdTokenExpiresAt,
                entity::setOidcIdTokenMetadata
        );
        if (oidcIdToken != null) {
            entity.setOidcIdTokenClaims(AuthUtil.writeMap(oidcIdToken.getClaims()));
        }

        OAuth2Authorization.Token<OAuth2UserCode> userCode =
                authorization.getToken(OAuth2UserCode.class);
        setTokenValues(
                userCode,
                entity::setUserCodeValue,
                entity::setUserCodeIssuedAt,
                entity::setUserCodeExpiresAt,
                entity::setUserCodeMetadata
        );

        OAuth2Authorization.Token<OAuth2DeviceCode> deviceCode =
                authorization.getToken(OAuth2DeviceCode.class);
        setTokenValues(
                deviceCode,
                entity::setDeviceCodeValue,
                entity::setDeviceCodeIssuedAt,
                entity::setDeviceCodeExpiresAt,
                entity::setDeviceCodeMetadata
        );

        // 设置登录ID
        String loginId = authorization.getAttribute(AuthConstants.SESSION_LOGIN_ID);
        if (StringUtils.hasText(loginId)) {
            entity.setLoginId(loginId);
        } else {
            WebUtil.getRequest().ifPresent(request -> {
                HttpSession session = request.getSession(false);
                if (Objects.nonNull(session)) {
                    entity.setLoginId((String) session.getAttribute(AuthConstants.SESSION_LOGIN_ID));
                }
            });
        }
        entity.setAttributes(AuthUtil.writeMap(authorization.getAttributes()));

        return entity;
    }

    private void setTokenValues(
            OAuth2Authorization.Token<?> token,
            Consumer<String> tokenValueConsumer,
            Consumer<Instant> issuedAtConsumer,
            Consumer<Instant> expiresAtConsumer,
            Consumer<String> metadataConsumer) {
        if (token != null) {
            OAuth2Token oAuth2Token = token.getToken();
            tokenValueConsumer.accept(oAuth2Token.getTokenValue());
            issuedAtConsumer.accept(oAuth2Token.getIssuedAt());
            expiresAtConsumer.accept(oAuth2Token.getExpiresAt());
            metadataConsumer.accept(AuthUtil.writeMap(token.getMetadata()));
        }
    }

    private static AuthorizationGrantType resolveAuthorizationGrantType(String authorizationGrantType) {
        if (AuthorizationGrantType.AUTHORIZATION_CODE.getValue().equals(authorizationGrantType)) {
            return AuthorizationGrantType.AUTHORIZATION_CODE;
        } else if (AuthorizationGrantType.CLIENT_CREDENTIALS.getValue().equals(authorizationGrantType)) {
            return AuthorizationGrantType.CLIENT_CREDENTIALS;
        } else if (AuthorizationGrantType.REFRESH_TOKEN.getValue().equals(authorizationGrantType)) {
            return AuthorizationGrantType.REFRESH_TOKEN;
        } else if (AuthorizationGrantType.DEVICE_CODE.getValue().equals(authorizationGrantType)) {
            return AuthorizationGrantType.DEVICE_CODE;
        }
        return new AuthorizationGrantType(authorizationGrantType);              // Custom authorization grant type
    }

    @SuppressWarnings("all")
    private void editOAuth2Authorization(OAuth2Authorization authorization) {
        boolean clearRefrshToken = false;
        boolean clearOAuth2AuthorizationCode = false;

        if (Objects.nonNull(authorization.getRefreshToken())) {
            String refreshTokenValue = authorization.getRefreshToken().getToken().getTokenValue();
            // 1. 检查在数据库中是否已存储 RefreshToken
            if (Objects.nonNull(authorizationRepository.findByRefreshTokenValue(refreshTokenValue))) {
                // 1.1 若数据库中已存储，则删除 RefreshToken
                clearRefrshToken = true;
            }
        }

        if (Objects.nonNull(authorization.getToken(OAuth2AuthorizationCode.class))) {
            String authorizationCodeValue = authorization.getToken(OAuth2AuthorizationCode.class).getToken().getTokenValue();
            // 2. 检查在数据库中是否已存储 OAuth2AuthorizationCode
            if (Objects.nonNull(authorizationRepository.findByAuthorizationCodeValue(authorizationCodeValue))) {
                // 2.1 若数据库中已存储，则删除 OAuth2AuthorizationCode
                clearOAuth2AuthorizationCode = true;
            }
        }

        // 3. 删除
        if (clearRefrshToken || clearOAuth2AuthorizationCode) {
            Field tokensField = ReflectionUtils.findField(OAuth2Authorization.class, "tokens");
            ReflectionUtils.makeAccessible(tokensField);
            var tokens = (Map<Class<? extends OAuth2Token>, OAuth2Authorization.Token<?>>) ReflectionUtils.getField(tokensField, authorization);
            if (Objects.nonNull(tokens)) {
                var editableMap = new HashMap<>(tokens);
                if (clearRefrshToken) {
                    editableMap.remove(OAuth2RefreshToken.class);
                }

                if (clearOAuth2AuthorizationCode) {
                    editableMap.remove(OAuth2AuthorizationCode.class);
                }
                ReflectionUtils.setField(tokensField, authorization, Collections.unmodifiableMap(editableMap));
            }
        }
    }
}
