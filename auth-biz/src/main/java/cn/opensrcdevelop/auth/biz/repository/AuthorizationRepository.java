package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.Authorization;

import java.util.List;

public interface AuthorizationRepository {

    Authorization findByStateOrAuthorizationCodeValueOrAccessTokenValueOrRefreshTokenValueOrOidcIdTokenValueOrUserCodeValueOrDeviceCodeValue(String token);

    Authorization findByState(String token);

    Authorization findByAuthorizationCodeValue(String token);

    Authorization findByAccessTokenValue(String token);

    Authorization findByRefreshTokenValue(String token);

    Authorization findByOidcIdTokenValue(String token);

    Authorization findByUserCodeValue(String token);

    Authorization findByDeviceCodeValue(String token);

    Authorization findById(Long id);

    void deleteById(Long id);

    void save(Authorization authorization);

    void deleteUserTokens(String principalName);

    List<Authorization> getAuthorizationsByClientId(String clientId);

    void deleteByIds(List<Long> ids);
}
