package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.OidcScope;

import java.util.List;

public interface OidcScopeRepository {

    List<OidcScope> searchScopeClaims(List<String> scopes);
}
