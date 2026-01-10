package cn.opensrcdevelop.auth.biz.repository.client.oidc.impl;

import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaim;
import cn.opensrcdevelop.auth.biz.mapper.client.oidc.OidcClaimMapper;
import cn.opensrcdevelop.auth.biz.repository.client.oidc.OidcClaimRepository;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class OidcClaimRepositoryImpl implements OidcClaimRepository {

    private final OidcClaimMapper oidcClaimMapper;

    /**
     * 检索 claim
     *
     * @param claims
     *            目标 claims
     * @return claims
     */
    @Override
    public List<OidcClaim> searchClaims(List<String> claims) {
        return oidcClaimMapper.searchClaims(claims);
    }
}
