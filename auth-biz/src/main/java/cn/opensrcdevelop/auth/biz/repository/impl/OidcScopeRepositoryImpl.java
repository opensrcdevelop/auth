package cn.opensrcdevelop.auth.biz.repository.impl;

import cn.opensrcdevelop.auth.biz.entity.OidcScope;
import cn.opensrcdevelop.auth.biz.mapper.OidcScopeMapper;
import cn.opensrcdevelop.auth.biz.repository.OidcScopeRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class OidcScopeRepositoryImpl implements OidcScopeRepository {

    private final OidcScopeMapper oidcScopeMapper;

    /**
     * 检索 scope
     *
     * @param scopes 目标 scopes
     * @return scopes
     */
    @Override
    public List<OidcScope> searchScopeClaims(List<String> scopes) {
        return oidcScopeMapper.searchScopeClaims(scopes);
    }
}
