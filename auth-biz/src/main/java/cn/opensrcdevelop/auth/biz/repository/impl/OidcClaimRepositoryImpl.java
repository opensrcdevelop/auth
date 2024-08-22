package cn.opensrcdevelop.auth.biz.repository.impl;

import cn.opensrcdevelop.auth.biz.entity.OidcClaim;
import cn.opensrcdevelop.auth.biz.mapper.OidcClaimMapper;
import cn.opensrcdevelop.auth.biz.repository.OidcClaimRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class OidcClaimRepositoryImpl implements OidcClaimRepository {

    private final OidcClaimMapper oidcClaimMapper;

    /**
     * 检索 claim
     *
     * @param claims 目标 claims
     * @return claims
     */
    @Override
    public List<OidcClaim> searchClaims(List<String> claims) {
        return oidcClaimMapper.searchClaims(claims);
    }
}
