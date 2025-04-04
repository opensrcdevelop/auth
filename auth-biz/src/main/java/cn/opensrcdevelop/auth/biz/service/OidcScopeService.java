package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcClaimScopeMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcScopeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcScopeResponseDto;
import cn.opensrcdevelop.auth.biz.entity.OidcScope;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface OidcScopeService extends IService<OidcScope> {

    void createOidcScope(OidcScopeRequestDto requestDto);

    void setOidcClaimScopeMapping(OidcClaimScopeMappingRequestDto requestDto);

    List<OidcScope> getScopeClaims(List<String> scopes);

    List<OidcScopeResponseDto> listScopes();

    void updateOidcScope(OidcScopeRequestDto requestDto);

    void removeOidcScope(String scopeId);
}
