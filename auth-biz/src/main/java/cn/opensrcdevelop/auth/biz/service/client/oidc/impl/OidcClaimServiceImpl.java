package cn.opensrcdevelop.auth.biz.service.client.oidc.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcClaimRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcClaimResponseDto;
import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaim;
import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaimScopeMapping;
import cn.opensrcdevelop.auth.biz.mapper.client.oidc.OidcClaimMapper;
import cn.opensrcdevelop.auth.biz.repository.client.oidc.OidcClaimRepository;
import cn.opensrcdevelop.auth.biz.service.client.oidc.OidcClaimScopeMappingService;
import cn.opensrcdevelop.auth.biz.service.client.oidc.OidcClaimService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;

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
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.OIDC_CLAIM,
            sysOperation = SysOperationType.CREATE,
            success = "'创建了 OIDC Claim（ ' + #requestDto.name + '），对应的用户属性为：' " +
                    " + @linkGen.toLink(#requestDto.userAttrId, T(ResourceType).USER_ATTR)",
            error = "'创建 OIDC Claim（' + #requestDto.name + '）失败，对应的用户属性为：' " +
                    " + @linkGen.toLink(#requestDto.userAttrId, T(ResourceType).USER_ATTR)"
    )
    @Transactional
    @Override
    public void createOidcClaim(OidcClaimRequestDto requestDto) {
        // 1. 检查
        checkClaimName(requestDto, null);

        // 2. 属性设置
        OidcClaim oidcClaim = new OidcClaim();
        oidcClaim.setClaimId(CommonUtil.getUUIDV7String());
        oidcClaim.setClaimName(requestDto.getName());
        oidcClaim.setUserAttrId(requestDto.getUserAttrId());

        // 3. 数据库操作
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
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.OIDC_CLAIM,
            sysOperation = SysOperationType.UPDATE,
            success = "'将 OIDC Claim 的名称由 ' + #oldName + ' 修改为了 ' + #requestDto.name" +
                    " + '，对应的用户属性由 '+ @linkGen.toLink(#oldUserAttrId, T(ResourceType).USER_ATTR) + ' 修改为了 '" +
                    " + @linkGen.toLink(#requestDto.userAttrId, T(ResourceType).USER_ATTR)",
            error = "'修改 OIDC Claim（' + #oldName + '）失败'"
    )
    @Transactional
    @Override
    public void updateOidcClaim(OidcClaimRequestDto requestDto) {
        String claimId = requestDto.getId();
        // 1. 获取原 OIDC Claim
        OidcClaim rawOidcClaim = super.getById(claimId);
        if (Objects.isNull(rawOidcClaim)) {
            return;
        }
        AuditContext.setSpelVariable("oldName", rawOidcClaim.getClaimName());
        AuditContext.setSpelVariable("oldUserAttrId", rawOidcClaim.getUserAttrId());

        // 2. 检查
        checkClaimName(requestDto, rawOidcClaim);

        // 3. 属性设置
        OidcClaim updateOidcClaim = new OidcClaim();
        updateOidcClaim.setClaimId(requestDto.getId());
        updateOidcClaim.setClaimName(requestDto.getName());
        updateOidcClaim.setUserAttrId(requestDto.getUserAttrId());

        // 4. 数据库操作
        super.updateById(updateOidcClaim);
    }

    /**
     * 删除 OIDC Claim
     *
     * @param claimId claim ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.OIDC_CLAIM,
            sysOperation = SysOperationType.DELETE,
            success = "'删除了 OIDC Claim（' + #claimName + '）'",
            error = "'删除 OIDC Claim（' + #claimName + '）失败'"
    )
    @Transactional
    @Override
    public void removeOidcClaim(String claimId) {
        // 1. 移除 scope claim 映射关系
        oidcClaimScopeMappingService.remove(Wrappers.<OidcClaimScopeMapping>lambdaQuery().eq(OidcClaimScopeMapping::getClaimId, claimId));

        AuditContext.setSpelVariable("claimName", super.getById(claimId).getClaimName());

        // 2. 移除 claim
        super.removeById(claimId);
    }

    private void checkClaimName(OidcClaimRequestDto requestDto, OidcClaim rawOidcClaim) {
        if (Objects.nonNull(rawOidcClaim) && StringUtils.equals(requestDto.getName(), rawOidcClaim.getClaimName())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<OidcClaim>lambdaQuery().eq(OidcClaim::getClaimName, requestDto.getName())))) {
            throw new BizException(MessageConstants.OIDC_CLAIM_MSG_1000, requestDto.getName());
        }
    }
}
