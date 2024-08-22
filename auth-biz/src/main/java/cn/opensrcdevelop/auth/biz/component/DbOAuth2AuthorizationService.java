package cn.opensrcdevelop.auth.biz.component;

import cn.opensrcdevelop.auth.biz.entity.Authorization;
import cn.opensrcdevelop.auth.biz.repository.AuthorizationRepository;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
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
import org.springframework.util.StringUtils;

import java.time.Instant;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

@Component
@Transactional
@Slf4j
public class DbOAuth2AuthorizationService implements OAuth2AuthorizationService {

    private final AuthorizationRepository authorizationRepository;
    private final RegisteredClientRepository registeredClientRepository;

    public DbOAuth2AuthorizationService(AuthorizationRepository authorizationRepository, RegisteredClientRepository registeredClientRepository) {
        Assert.notNull(authorizationRepository, "authorizationRepository cannot be null");
        Assert.notNull(registeredClientRepository, "registeredClientRepository cannot be null");
        this.authorizationRepository = authorizationRepository;
        this.registeredClientRepository = registeredClientRepository;
    }

    @Override
    public void save(OAuth2Authorization authorization) {
        Assert.notNull(authorization, "authorization cannot be null");
        this.authorizationRepository.save(toEntity(authorization));
        clearExpiredTokens(authorization.getRegisteredClientId());
    }

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
                .attributes(attributes -> attributes.putAll(AuthUtil.parseMap(entity.getAttributes())));
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
        entity.setAttributes(AuthUtil.writeMap(authorization.getAttributes()));
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

    /**
     * 清除过期 token
     *
     * @param clientId 客户端 ID
     */
    private void clearExpiredTokens(String clientId) {
        log.info("开始清除过期 token");
        // 1. 获取客户端下所有授权的 token
        var oauth2AuthorizationList = CommonUtil.stream(authorizationRepository.getAuthorizationsByClientId(clientId)).map(this::toObject).collect(Collectors.toCollection(ArrayList::new));

        List<OAuth2Authorization> deleteTargetList = new ArrayList<>();
        if (CollectionUtils.isEmpty(oauth2AuthorizationList)) {
            log.info("结束清除过期 token");
            return;
        }

        // 2. 清除 token，生命周期由长到短依次清除（refresh_token → access_token → authorization_code）
        // 2.1 清除 refresh_token 过期的记录
        oauth2AuthorizationList.forEach(authorization -> {
            if (Objects.nonNull(authorization.getRefreshToken()) && authorization.getRefreshToken().isExpired()) {
                deleteTargetList.add(authorization);
            }
        });
        oauth2AuthorizationList.removeAll(deleteTargetList);

        // 2.2 清除 access_token 过期的记录
        // 2.2.1. 清除无 refresh_token，access_token 过期的记录
        oauth2AuthorizationList.forEach(authorization -> {
            if (Objects.nonNull(authorization.getAccessToken()) && Objects.isNull(authorization.getRefreshToken()) && authorization.getAccessToken().isExpired()) {
                deleteTargetList.add(authorization);
            }
        });
        oauth2AuthorizationList.removeAll(deleteTargetList);

        // 2.2.2 清除有 refresh_token，access_token 过期的记录
        // 2.2.2.1 按 refresh_token 分组，清除 access_token 过期的记录
        var groupRes = CommonUtil.stream(oauth2AuthorizationList)
                .filter(o -> Objects.nonNull(o.getRefreshToken()))
                .collect(Collectors.groupingBy(OAuth2Authorization::getRefreshToken));
        if (MapUtils.isNotEmpty(groupRes)) {
            // 2.2.2.1.1 分组内按 ID 排序
            groupRes.keySet().forEach(key -> groupRes.computeIfPresent(key, (k, v) -> v.stream().sorted(Comparator.comparing(OAuth2Authorization::getId)).collect(Collectors.toCollection(ArrayList::new))));
            // 2.2.2.1.2 保留分组内最新的一条记录，即保留 refresh_token
            groupRes.keySet().forEach(key -> groupRes.computeIfPresent(key, (k, v) -> {
                if (!v.isEmpty()) {
                    v.remove(v.size() - 1);
                }
                return v;
            }));

            // 2.2.2.1.3 清除 access_token 过期的记录
            CommonUtil.stream(groupRes.values()).forEach(authorizationList -> {
                authorizationList.forEach(authorization -> {
                    if (Objects.nonNull(authorization.getAccessToken()) && authorization.getAccessToken().isExpired()) {
                        deleteTargetList.add(authorization);
                    }
                });
            });
            oauth2AuthorizationList.removeAll(deleteTargetList);
        }

        // 2.3 清除 authorization_code 过期的记录
        oauth2AuthorizationList.forEach(authorization -> {
            if (Objects.isNull(authorization.getAccessToken()) && Objects.isNull(authorization.getRefreshToken())) {
                var code = authorization.getToken(OAuth2AuthorizationCode.class);
                if (Objects.nonNull(code) && code.isExpired()) {
                    deleteTargetList.add(authorization);
                }
            }
        });
        oauth2AuthorizationList.removeAll(deleteTargetList);

        // 3. 数据库操作
        var deleteTargetIds = CommonUtil.stream(deleteTargetList).map(OAuth2Authorization::getId).map(Long::parseLong).toList();
        log.info("清除的过期 token 数：{}", deleteTargetIds.size());
        if (CollectionUtils.isNotEmpty(deleteTargetIds)) {
            authorizationRepository.deleteByIds(deleteTargetIds);
        }
        log.info("结束清除过期 token");
    }
}
