package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcClaimRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcClaimResponseDto;
import cn.opensrcdevelop.auth.biz.entity.OidcClaim;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface OidcClaimService extends IService<OidcClaim> {

    void createOidcClaim(OidcClaimRequestDto requestDto);

    List<OidcClaimResponseDto> listClaims();

    void updateOidcClaim(OidcClaimRequestDto requestDto);

    void removeOidcClaim(String claimId);
}
