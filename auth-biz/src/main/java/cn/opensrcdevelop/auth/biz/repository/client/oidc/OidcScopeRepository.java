package cn.opensrcdevelop.auth.biz.repository.client.oidc;

import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcScope;
import java.util.List;

public interface OidcScopeRepository {

    List<OidcScope> searchScopeClaims(List<String> scopes);
}
