package cn.opensrcdevelop.auth.biz.service.client.oidc.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcClaimScopeMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcScopeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcScopeResponseDto;
import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaim;
import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaimScopeMapping;
import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcScope;
import cn.opensrcdevelop.auth.biz.mapper.client.oidc.OidcScopeMapper;
import cn.opensrcdevelop.auth.biz.repository.client.oidc.OidcScopeRepository;
import cn.opensrcdevelop.auth.biz.service.client.oidc.OidcClaimScopeMappingService;
import cn.opensrcdevelop.auth.biz.service.client.oidc.OidcClaimService;
import cn.opensrcdevelop.auth.biz.service.client.oidc.OidcScopeService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Strings;
import org.springframework.aop.framework.AopContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class OidcScopeServiceImpl extends ServiceImpl<OidcScopeMapper, OidcScope> implements OidcScopeService {

    private final OidcClaimScopeMappingService oidcClaimScopeMappingService;
    private final OidcScopeRepository oidcScopeRepository;
    private final OidcClaimService oidcClaimService;

    /**
     * 创建 OIDC scope
     *
     * @param requestDto
     *            创建 OIDC scope 请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.OIDC_SCOPE, sysOperation = SysOperationType.CREATE, success = "创建了 OIDC Scope（{{ #requestDto.name }}），包含的 Claim 为：{{ #claims }}", fail = "创建 OIDC Scope（{{ #requestDto.name }}）失败，包含的 Claim 为：{{ #claims }}")
    @Transactional
    @Override
    public void createOidcScope(OidcScopeRequestDto requestDto) {
        // 1. 检查
        checkScopeName(requestDto, null);

        // 2. 属性编辑
        OidcScope oidcScope = new OidcScope();
        String scopeId = CommonUtil.getUUIDV7String();
        oidcScope.setScopeId(scopeId);
        oidcScope.setScopeName(requestDto.getName());

        var claimIds = requestDto.getClaims();
        AuditContext.setSpelVariable("claims", getClaimNames(claimIds));

        // 3. 数据库操作
        super.save(oidcScope);

        // 4. 设置 claim 映射关系
        if (CollectionUtils.isNotEmpty(claimIds)) {
            OidcClaimScopeMappingRequestDto claimScopeMappingRequestDto = new OidcClaimScopeMappingRequestDto();
            claimScopeMappingRequestDto.setScopeId(scopeId);
            claimScopeMappingRequestDto.setClaimIds(claimIds);
            ((OidcScopeService) AopContext.currentProxy()).setOidcClaimScopeMapping(claimScopeMappingRequestDto);
        }
    }

    /**
     * 设置 OIDC claim scope 映射关系
     *
     * @param requestDto
     *            设置 OIDC claim scope 映射关系请求
     */
    @Transactional
    @Override
    public void setOidcClaimScopeMapping(OidcClaimScopeMappingRequestDto requestDto) {
        // 1. 删除原有的映射关系
        String scopeId = requestDto.getScopeId();
        oidcClaimScopeMappingService
                .remove(Wrappers.<OidcClaimScopeMapping>lambdaQuery().eq(OidcClaimScopeMapping::getScopeId, scopeId));

        // 2. 属性编辑
        var mappings = CommonUtil.stream(requestDto.getClaimIds()).map(claimId -> {
            OidcClaimScopeMapping mapping = new OidcClaimScopeMapping();
            mapping.setScopeId(scopeId);
            mapping.setClaimId(claimId);
            return mapping;
        }).toList();

        // 3. 数据库操作
        oidcClaimScopeMappingService.saveBatch(mappings);
    }

    /**
     * 获取 OIDC scope claim 映射关系
     *
     * @param scopes
     *            OIDC scope 名
     * @return OIDC scope 集合
     */
    @Override
    public List<OidcScope> getScopeClaims(List<String> scopes) {
        return oidcScopeRepository.searchScopeClaims(scopes);
    }

    /**
     * 获取所有 OIDC Scope
     *
     * @return 所有 OIDC Scope
     */
    @Override
    public List<OidcScopeResponseDto> listScopes() {
        // 1. 数据库操作
        List<OidcScope> scopes = oidcScopeRepository.searchScopeClaims(null);

        // 2. 属性编辑
        return CommonUtil.stream(scopes).map(scope -> {
            OidcScopeResponseDto scopeResponse = new OidcScopeResponseDto();
            scopeResponse.setId(scope.getScopeId());
            scopeResponse.setName(scope.getScopeName());

            List<String> claims = new ArrayList<>();
            CommonUtil.stream(scope.getClaims()).forEach(claim -> claims.add(claim.getClaimId()));
            scopeResponse.setClaims(claims);
            return scopeResponse;
        }).sorted(Comparator.comparing(OidcScopeResponseDto::getName)).toList();
    }

    /**
     * 更新 OIDC scope
     *
     * @param requestDto
     *            更新 OIDC scope 请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.OIDC_SCOPE, sysOperation = SysOperationType.UPDATE, success = "将 OIDC Scope 的名称由 {{ #oldName }} 修改为了 {{ #requestDto.name }}，包含的 Claim 由 {{ #oldClaims }} 修改为了 {{ #claims }}", fail = "修改 OIDC Scope（{{ #oldName }}）失败")
    @Transactional
    @Override
    public void updateOidcScope(OidcScopeRequestDto requestDto) {
        String scopeId = requestDto.getId();
        // 1. 获取原 OIDC Scope
        OidcScope rawOidcScope = super.getById(scopeId);
        if (Objects.isNull(rawOidcScope)) {
            return;
        }
        AuditContext.setSpelVariable("oldName", rawOidcScope.getScopeName());
        AuditContext.setSpelVariable("oldClaims", getClaimNames(CommonUtil
                .stream(oidcClaimScopeMappingService
                        .list(Wrappers.<OidcClaimScopeMapping>lambdaQuery().eq(OidcClaimScopeMapping::getScopeId,
                                scopeId)))
                .map(OidcClaimScopeMapping::getClaimId).toList()));

        // 2. 检查
        checkScopeName(requestDto, rawOidcScope);

        // 3. 更新 scope
        super.update(Wrappers.<OidcScope>lambdaUpdate()
                .set(OidcScope::getScopeName, requestDto.getName())
                .eq(OidcScope::getScopeId, requestDto.getId()));

        // 4. 设置 claim 映射关系
        var claimIds = requestDto.getClaims();
        if (CollectionUtils.isNotEmpty(claimIds)) {
            OidcClaimScopeMappingRequestDto claimScopeMappingRequestDto = new OidcClaimScopeMappingRequestDto();
            claimScopeMappingRequestDto.setScopeId(scopeId);
            claimScopeMappingRequestDto.setClaimIds(claimIds);
            ((OidcScopeService) AopContext.currentProxy()).setOidcClaimScopeMapping(claimScopeMappingRequestDto);
        }

        AuditContext.setSpelVariable("claims", getClaimNames(claimIds));
    }

    /**
     * 删除 OIDC scope
     *
     * @param scopeId
     *            scope ID
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.OIDC_SCOPE, sysOperation = SysOperationType.DELETE, success = "删除了 OIDC Scope（{{ #scopeName }}）", fail = "删除 OIDC Scope（{{ #scopeName }}）失败")
    @Transactional
    @Override
    public void removeOidcScope(String scopeId) {
        if (StringUtils.isNotEmpty(scopeId)) {
            // 1. 移除 scope claim 映射关系
            oidcClaimScopeMappingService.remove(
                    Wrappers.<OidcClaimScopeMapping>lambdaQuery().eq(OidcClaimScopeMapping::getScopeId, scopeId));

            AuditContext.setSpelVariable("scopeName", super.getById(scopeId).getScopeName());

            // 2. 移除 scope
            super.removeById(scopeId);
        }
    }

    private void checkScopeName(OidcScopeRequestDto requestDto, OidcScope rawOidcScope) {
        if (Objects.nonNull(rawOidcScope) && Strings.CS.equals(requestDto.getName(), rawOidcScope.getScopeName())) {
            return;
        }

        if (Objects.nonNull(
                super.getOne(Wrappers.<OidcScope>lambdaQuery().eq(OidcScope::getScopeName, requestDto.getName())))) {
            throw new BizException(MessageConstants.OIDC_SCOPE_MSG_1000, requestDto.getName());
        }
    }

    private List<String> getClaimNames(List<String> claimIds) {
        var claims = oidcClaimService.list(Wrappers.<OidcClaim>lambdaQuery().in(OidcClaim::getClaimId, claimIds));
        return CommonUtil.stream(claims).map(OidcClaim::getClaimName).toList();
    }
}
