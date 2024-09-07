package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.dto.OidcClaimScopeMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.OidcScopeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.OidcScopeResponseDto;
import cn.opensrcdevelop.auth.biz.entity.OidcClaimScopeMapping;
import cn.opensrcdevelop.auth.biz.entity.OidcScope;
import cn.opensrcdevelop.auth.biz.mapper.OidcScopeMapper;
import cn.opensrcdevelop.auth.biz.repository.OidcScopeRepository;
import cn.opensrcdevelop.auth.biz.service.OidcClaimScopeMappingService;
import cn.opensrcdevelop.auth.biz.service.OidcScopeService;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.aop.framework.AopContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Service
@RequiredArgsConstructor
public class OidcScopeServiceImpl extends ServiceImpl<OidcScopeMapper, OidcScope> implements OidcScopeService {

    private final OidcClaimScopeMappingService oidcClaimScopeMappingService;
    private final OidcScopeRepository oidcScopeRepository;

    /**
     * 创建 OIDC scope
     *
     * @param requestDto 创建 OIDC scope 请求
     */
    @Transactional
    @Override
    public void createOidcScope(OidcScopeRequestDto requestDto) {
        // 1. 属性编辑
        OidcScope oidcScope = new OidcScope();
        String scopeId = CommonUtil.getUUIDString();
        oidcScope.setScopeId(scopeId);
        oidcScope.setScopeName(requestDto.getName());

        // 2. 数据库操作
        super.save(oidcScope);

        // 3. 设置 claim 映射关系
        var claimIds = requestDto.getClaims();
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
     * @param requestDto 设置 OIDC claim scope 映射关系请求
     */
    @Transactional
    @Override
    public void setOidcClaimScopeMapping(OidcClaimScopeMappingRequestDto requestDto) {
        // 1. 删除原有的映射关系
        String scopeId = requestDto.getScopeId();
        oidcClaimScopeMappingService.remove(Wrappers.<OidcClaimScopeMapping>lambdaQuery().eq(OidcClaimScopeMapping::getScopeId, scopeId));

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
     * @param scopes OIDC scope 名
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
     * @param requestDto 更新 OIDC scope 请求
     */
    @Transactional
    @Override
    public void updateOidcScope(OidcScopeRequestDto requestDto) {
        // 1. 更新 scope
        String scopeId = requestDto.getId();
        super.update(Wrappers.<OidcScope>lambdaUpdate()
                .set(OidcScope::getScopeName, requestDto.getName())
                .eq(OidcScope::getScopeId, requestDto.getId()));

        // 2. 设置 claim 映射关系
        var claimIds = requestDto.getClaims();
        if (CollectionUtils.isNotEmpty(claimIds)) {
            OidcClaimScopeMappingRequestDto claimScopeMappingRequestDto = new OidcClaimScopeMappingRequestDto();
            claimScopeMappingRequestDto.setScopeId(scopeId);
            claimScopeMappingRequestDto.setClaimIds(claimIds);
            ((OidcScopeService) AopContext.currentProxy()).setOidcClaimScopeMapping(claimScopeMappingRequestDto);
        }
    }

    /**
     * 删除 OIDC scope
     *
     * @param scopeId scope ID
     */
    @Transactional
    @Override
    public void removeOidcScope(String scopeId) {
        if (StringUtils.isNotEmpty(scopeId)) {
            // 1. 移除 scope claim 映射关系
            oidcClaimScopeMappingService.remove(Wrappers.<OidcClaimScopeMapping>lambdaQuery().eq(OidcClaimScopeMapping::getScopeId, scopeId));

            // 2. 移除 scope
            super.removeById(scopeId);
        }
    }
}
