package cn.opensrcdevelop.auth.biz.repository.auth;

import cn.opensrcdevelop.auth.biz.entity.auth.Authorization;
import java.util.List;

public interface AuthorizationRepository {

    Authorization findByStateOrAuthorizationCodeValueOrAccessTokenValueOrRefreshTokenValueOrOidcIdTokenValueOrUserCodeValueOrDeviceCodeValue(
            String token);

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

    void deleteByLoginId(String loginId);

    List<Authorization> findRefreshTokensByPrincipalName(String principalName);

    List<Authorization> findRefreshTokensByLoginId(String loginId);
}
