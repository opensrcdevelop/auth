package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.PrincipalTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.AuthorizeRecordResponseDto;
import cn.opensrcdevelop.auth.biz.dto.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.PermissionRequestDto;
import cn.opensrcdevelop.auth.biz.dto.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.entity.*;
import cn.opensrcdevelop.auth.biz.mapper.PermissionMapper;
import cn.opensrcdevelop.auth.biz.repository.PermissionRepository;
import cn.opensrcdevelop.auth.biz.service.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.PermissionService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.cache.annoation.CacheExpire;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.vavr.Tuple;
import io.vavr.Tuple4;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PermissionServiceImpl extends ServiceImpl<PermissionMapper, Permission> implements PermissionService {

    private final PermissionRepository permissionRepository;
    private final AuthorizeService authorizeService;

    /**
     * 创建权限
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createPermission(PermissionRequestDto requestDto) {
        // 1.属性设置
        Permission permission = new Permission();
        permission.setPermissionName(requestDto.getName());
        permission.setPermissionCode(requestDto.getCode());
        permission.setDescription(requestDto.getDesc());
        permission.setPermissionId(CommonUtil.getUUIDString());
        permission.setResourceId(requestDto.getResourceId());

        // 2. 数据库操作
        super.save(permission);
    }

    /**
     * 获取当前用户权限
     *
     * @return 当前用户权限
     */
    @Cacheable(
            cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS,
            key = "#root.target.generateCurrentUserPermissionsCacheKey()",
            condition = "#root.target.generateCurrentUserPermissionsCacheCondition()")
    @CacheExpire("30 * 60")
    @Override
    public List<PermissionResponseDto> getCurrentUserPermissions() {
        // 1. 获取当前用户
        String userId = AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB);
        List<String> aud = AuthUtil.getCurrentJwtClaim(JwtClaimNames.AUD);

        if (StringUtils.isNotEmpty(userId) && CollectionUtils.isNotEmpty(aud)) {
            // 2. 数据库操作
            List<AuthorizeRecord> authorizeRecords = getUserPermissions(userId, aud.get(0));
            // 2.1 过滤重复的权限（按授权先后顺序）
            var records = CommonUtil.stream(authorizeRecords).collect(Collectors
                    .collectingAndThen(Collectors.toCollection(() -> new TreeSet<>(Comparator.comparing(r -> {
                        Permission permission = r.getPermission();
                        return permission.getResource().getResourceCode() + permission.getPermissionCode();
                    }))), ArrayList::new));

            // 3. 响应数据设置
            return CommonUtil.stream(records).map(authorizeRecord -> {
                PermissionResponseDto response = new PermissionResponseDto();

                // 3.1 权限信息
                Permission permission = authorizeRecord.getPermission();
                response.setPermissionCode(permission.getPermissionCode());
                response.setResourceCode(permission.getResource().getResourceCode());

                // 3.2 限制条件
                var conditions =  CommonUtil.stream(authorizeRecord.getPermissionExps()).map(exp -> {
                    PermissionExpResponseDto permissionExpResponse = new PermissionExpResponseDto();
                    permissionExpResponse.setExpression(exp.getExpression());
                    return permissionExpResponse;
                }).toList();
                response.setConditions(conditions);
                return response;
            }).toList();
        }
        return Collections.emptyList();
    }

    /**
     * 获取用户权限
     *
     * @param userId 用户ID
     * @param resourceGroupCode 资源组码
     * @return 用户权限
     */
    @Override
    public List<AuthorizeRecord> getUserPermissions(String userId, String resourceGroupCode) {
        return permissionRepository.searchUserPermissions(userId, resourceGroupCode);
    }

    /**
     * 获取用户组权限
     *
     * @param userGroupId 用户组ID
     * @return 用户组权限
     */
    @Override
    public List<AuthorizeRecord> getUserGroupPermissions(String userGroupId) {
        return permissionRepository.searchUserGroupPermissions(userGroupId);
    }

    /**
     * 获取角色权限
     *
     * @param roleId 角色ID
     * @return 角色权限
     */
    @Override
    public List<AuthorizeRecord> getRolePermissions(String roleId) {
        return permissionRepository.searchRolePermissions(roleId);
    }

    /**
     * 获取资源内权限
     *
     * @param page 分页对象
     * @param resourceId 资源ID
     * @param keyword 资源名称 / 标识搜索关键字
     */
    @Override
    public void getResourcePermissions(IPage<Permission> page, String resourceId, String keyword) {
        // 1. 查询数据库
        var query = Wrappers.<Permission>lambdaQuery().eq(Permission::getResourceId, resourceId).orderByAsc(Permission::getPermissionCode);
        if (StringUtils.isNotEmpty(keyword)) {
            query = query.and(o -> o.like(Permission::getPermissionName, keyword).or(i -> i.like(Permission::getPermissionCode, keyword))).orderByAsc(Permission::getPermissionCode);
        }
        var permissions = super.list(page, query);

        // 2. 设置权限列表
        page.setRecords(permissions);
    }

    /**
     * 获取权限详情
     *
     * @param permissionId 权限ID
     * @param keyword 被授权主体关键字
     * @return 权限详情
     */
    @Override
    public PermissionResponseDto detail(String permissionId, String keyword) {
        PermissionResponseDto permissionResponse = new PermissionResponseDto();
        // 1. 查询数据库
        Permission permission = super.getById(permissionId);
        if (permission == null) {
            return permissionResponse;
        }

        // 2. 设置基本信息
        permissionResponse.setPermissionId(permission.getPermissionId());
        permissionResponse.setPermissionName(permission.getPermissionName());
        permissionResponse.setPermissionCode(permission.getPermissionCode());
        permissionResponse.setPermissionDesc(permission.getDescription());

        // 3. 设置授权记录
        var records = CommonUtil.stream(permissionRepository.searchPermissionAuthorizeRecords(permissionId, keyword)).map(authorizeRecord -> {
            AuthorizeRecordResponseDto authorizeRecordResponse = new AuthorizeRecordResponseDto();
            authorizeRecordResponse.setAuthorizeId(authorizeRecord.getAuthorizeId());
            authorizeRecordResponse.setAuthorizeTime(authorizeRecord.getAuthorizeTime());

            // 3.1 授权条件
            var conditions = CommonUtil.stream(authorizeRecord.getPermissionExps()).map(exp -> {
                PermissionExpResponseDto condition = new PermissionExpResponseDto();
                condition.setId(exp.getExpressionId());
                condition.setName(exp.getExpressionName());
                condition.setExpression(exp.getExpression());
                return condition;
            }).toList();
            authorizeRecordResponse.setConditions(conditions);

            // 3.2 授权主体和类型
            var principal = getPrincipal(authorizeRecord);
            authorizeRecordResponse.setPrincipalId(principal._1);
            authorizeRecordResponse.setPrincipal(principal._2);
            authorizeRecordResponse.setPrincipalType(principal._3);
            authorizeRecordResponse.setPrincipalTypeDisplayName(principal._4);

            return authorizeRecordResponse;
        }).toList();
        permissionResponse.setAuthorizeRecords(records);

        return permissionResponse;
    }

    /**
     * 删除资源下的权限
     *
     * @param resourceIds 资源ID集合
     */
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void removeResourcePermissions(List<String> resourceIds) {
        // 1. 查询资源下的待删除权限
        var query = Wrappers.<Permission>lambdaQuery().in(Permission::getResourceId, resourceIds);
        var permissions = super.list(query);

        if (CollectionUtils.isNotEmpty(permissions)) {
            // 2. 删除资源下的所有权限
            super.remove(query);

            // 3. 删除权限有关的所有授权记录
            var ids = permissions.stream().map(Permission::getPermissionId).toList();
            authorizeService.removeAuthorization(ids);
        }
    }

    /**
     * 删除权限
     *
     * @param permissionId 权限ID
     */
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void removePermission(String permissionId) {
        // 1. 删除权限
        super.remove(Wrappers.<Permission>lambdaQuery().eq(Permission::getPermissionId, permissionId));

        // 2. 删除权限有关的所有授权记录
        authorizeService.removeAuthorization(List.of(permissionId));
    }

    /**
     * 更新权限
     *
     * @param requestDto 请求
     */
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void updatePermission(PermissionRequestDto requestDto) {
        // 1. 获取版本号
        var rawPermission = super.getById(requestDto.getId());
        if (Objects.isNull(rawPermission)) {
            return;
        }

        // 2. 属性设置
        Permission updatePermission = new Permission();
        updatePermission.setPermissionId(requestDto.getId());
        updatePermission.setPermissionName(requestDto.getName());
        updatePermission.setPermissionCode(requestDto.getCode());
        updatePermission.setDescription(requestDto.getDesc());
        updatePermission.setVersion(rawPermission.getVersion());

        // 3. 数据库操作
        super.updateById(updatePermission);
    }

    /**
     * 获取权限表达式关联的权限
     *
     * @param expressionId 表达式ID
     * @return 权限表达式关联的授权
     */
    @Override
    public List<AuthorizeRecord> getExpPermissions(String expressionId) {
        return permissionRepository.searchExpPermission(expressionId);
    }

    private Tuple4<String, String, String, String> getPrincipal(AuthorizeRecord authorizeRecord) {
        User user = authorizeRecord.getUser();
        UserGroup userGroup = authorizeRecord.getUserGroup();
        Role role = authorizeRecord.getRole();

        if (user != null) {
            return Tuple.of(user.getUserId(), user.getUsername(), PrincipalTypeEnum.USER.getType(), PrincipalTypeEnum.USER.getDisplayName());
        }

        if (userGroup != null) {
            return Tuple.of(userGroup.getUserGroupId(), userGroup.getUserGroupName(), PrincipalTypeEnum.USER_GROUP.getType(), PrincipalTypeEnum.USER_GROUP.getDisplayName());
        }

        if (role != null) {
            return Tuple.of(role.getRoleId(), role.getRoleName(), PrincipalTypeEnum.ROLE.getType(), PrincipalTypeEnum.ROLE.getDisplayName());
        }
        return Tuple.of(null, null, null, null);
    }

    /**
     * 生成 Redis 缓存 key
     *
     * @return Redis 缓存 key
     */
    public String generateCurrentUserPermissionsCacheKey() {
        String userId = AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB);
        List<String> aud = AuthUtil.getCurrentJwtClaim(JwtClaimNames.AUD);
        Objects.requireNonNull(aud);
        return TenantContextHolder.getTenantContext().getTenantCode() + ":" + userId + ":" + aud.get(0);
    }

    /**
     * 生成 Redis 缓存条件
     *
     * @return Redis 缓存条件
     */
    public boolean generateCurrentUserPermissionsCacheCondition() {
        return Objects.nonNull(AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB)) &&
                CollectionUtils.isNotEmpty(AuthUtil.getCurrentJwtClaim(JwtClaimNames.AUD));
    }
}
