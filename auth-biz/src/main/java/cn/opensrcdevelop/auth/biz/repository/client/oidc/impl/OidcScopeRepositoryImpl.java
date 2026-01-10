package cn.opensrcdevelop.auth.biz.repository.client.oidc.impl;

import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcScope;
import cn.opensrcdevelop.auth.biz.mapper.client.oidc.OidcScopeMapper;
import cn.opensrcdevelop.auth.biz.repository.client.oidc.OidcScopeRepository;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class OidcScopeRepositoryImpl implements OidcScopeRepository {

    private final OidcScopeMapper oidcScopeMapper;

    /**
     * 检索 scope
     *
     * @param scopes
     *            目标 scopes
     * @return scopes
     */
    @Override
    public List<OidcScope> searchScopeClaims(List<String> scopes) {
        return oidcScopeMapper.searchScopeClaims(scopes);
    }
}
