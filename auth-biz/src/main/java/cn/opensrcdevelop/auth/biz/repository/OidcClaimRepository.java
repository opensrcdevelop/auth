package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.OidcClaim;

import java.util.List;

public interface OidcClaimRepository {

    List<OidcClaim> searchClaims(List<String> claims);
}
