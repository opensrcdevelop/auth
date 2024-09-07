package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.dto.OidcClaimRequestDto;
import cn.opensrcdevelop.auth.biz.dto.OidcClaimResponseDto;
import cn.opensrcdevelop.auth.biz.entity.OidcClaim;
import cn.opensrcdevelop.auth.biz.entity.OidcClaimScopeMapping;
import cn.opensrcdevelop.auth.biz.mapper.OidcClaimMapper;
import cn.opensrcdevelop.auth.biz.repository.OidcClaimRepository;
import cn.opensrcdevelop.auth.biz.service.OidcClaimScopeMappingService;
import cn.opensrcdevelop.auth.biz.service.OidcClaimService;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;

@Service
@RequiredArgsConstructor
public class OidcClaimServiceImpl extends ServiceImpl<OidcClaimMapper, OidcClaim> implements OidcClaimService {

    private final OidcClaimRepository oidcClaimRepository;
    private final OidcClaimScopeMappingService oidcClaimScopeMappingService;

    /**
     * 设置 OIDC claim
     *
     * @param requestDto 设置 OIDC claim 请求
     */
    @Transactional
    @Override
    public void createOidcClaim(OidcClaimRequestDto requestDto) {
        // 1. 属性设置
        OidcClaim oidcClaim = new OidcClaim();
        oidcClaim.setClaimId(CommonUtil.getUUIDString());
        oidcClaim.setClaimName(requestDto.getName());
        oidcClaim.setUserAttrId(requestDto.getUserAttrId());

        // 2. 数据库操作
        super.save(oidcClaim);
    }

    /**
     * 获取所有 OIDC Claim
     *
     * @return 所有 OIDC Claim
     */
    @Override
    public List<OidcClaimResponseDto> listClaims() {
        // 1. 数据库操作
        var claims = oidcClaimRepository.searchClaims(null);

        // 2. 属性设置
        return CommonUtil.stream(claims).map(claim -> {
            OidcClaimResponseDto claimResponse = new OidcClaimResponseDto();
            claimResponse.setId(claim.getClaimId());
            claimResponse.setName(claim.getClaimName());
            claimResponse.setUserAttrId(claim.getUserAttr().getAttrId());

            return claimResponse;
        }).sorted(Comparator.comparing(OidcClaimResponseDto::getName)).toList();
    }

    /**
     * 更新 OIDC claim
     *
     * @param requestDto 更新 OIDC claim 请求
     */
    @Transactional
    @Override
    public void updateOidcClaim(OidcClaimRequestDto requestDto) {
        // 1. 属性设置
        OidcClaim updateOidcClaim = new OidcClaim();
        updateOidcClaim.setClaimId(requestDto.getId());
        updateOidcClaim.setClaimName(requestDto.getName());
        updateOidcClaim.setUserAttrId(requestDto.getUserAttrId());

        // 2. 数据库操作
        super.updateById(updateOidcClaim);
    }

    /**
     * 删除 OIDC Claim
     *
     * @param claimId claim ID
     */
    @Transactional
    @Override
    public void removeOidcClaim(String claimId) {
        // 1. 移除 scope claim 映射关系
        oidcClaimScopeMappingService.remove(Wrappers.<OidcClaimScopeMapping>lambdaQuery().eq(OidcClaimScopeMapping::getClaimId, claimId));

        // 2. 移除 claim
        super.removeById(claimId);
    }
}
