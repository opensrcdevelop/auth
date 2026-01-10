package cn.opensrcdevelop.auth.biz.repository.client.oidc;

import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaim;
import java.util.List;

public interface OidcClaimRepository {

    List<OidcClaim> searchClaims(List<String> claims);
}
