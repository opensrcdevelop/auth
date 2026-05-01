package cn.opensrcdevelop.auth.biz.service.permission.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.*;
import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeRecordResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.*;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.role.RoleResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.UserResponseDto;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.permission.Permission;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequest;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequestItem;
import cn.opensrcdevelop.auth.biz.entity.resource.Resource;
import cn.opensrcdevelop.auth.biz.entity.resource.group.ResourceGroup;
import cn.opensrcdevelop.auth.biz.entity.role.Role;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroup;
import cn.opensrcdevelop.auth.biz.mapper.permission.PermissionMapper;
import cn.opensrcdevelop.auth.biz.repository.permission.PermissionRepository;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.permission.PermissionService;
import cn.opensrcdevelop.auth.biz.service.permission.expression.PermissionExpService;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestItemService;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestService;
import cn.opensrcdevelop.auth.biz.service.resource.ResourceService;
import cn.opensrcdevelop.auth.biz.service.role.RoleService;
import cn.opensrcdevelop.auth.biz.service.user.group.UserGroupService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.vavr.Tuple;
import io.vavr.Tuple4;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Strings;
import org.springframework.aop.framework.AopContext;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
public class PermissionServiceImpl extends ServiceImpl<PermissionMapper, Permission> implements PermissionService {

    private final PermissionRepository permissionRepository;
    private final AuthorizeService authorizeService;
    private final PermissionRequestService permissionRequestService;
    private final PermissionRequestItemService permissionRequestItemService;
    private final ResourceService resourceService;
    private final PermissionExpService permissionExpService;
    private final UserGroupService userGroupService;
    private final RoleService roleService;

    public PermissionServiceImpl(
            PermissionRepository permissionRepository,
            @Lazy AuthorizeService authorizeService,
            @Lazy ResourceService resourceService,
            @Lazy PermissionExpService permissionExpService,
            @Lazy UserGroupService userGroupService,
            @Lazy PermissionRequestService permissionRequestService,
            PermissionRequestItemService permissionRequestItemService,
            @Lazy RoleService roleService) {
        this.permissionRepository = permissionRepository;
        this.authorizeService = authorizeService;
        this.resourceService = resourceService;
        this.permissionExpService = permissionExpService;
        this.userGroupService = userGroupService;
        this.permissionRequestService = permissionRequestService;
        this.permissionRequestItemService = permissionRequestItemService;
        this.roleService = roleService;
    }

    /**
     * 创建权限
     *
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION, sysOperation = SysOperationType.CREATE, success = "在资源（{{ @linkGen.toLink(#requestDto.resourceId, T(ResourceType).RESOURCE) }}）中创建了权限（ "
            +
            "{{ @linkGen.toLink(#permissionId, T(ResourceType).PERMISSION) }}）", fail = "在资源（{{ @linkGen.toLink(#requestDto.resourceId, T(ResourceType).RESOURCE) }}）中创建权限（ "
                    +
                    "{{ #requestDto.name }}）失败")
    @Transactional
    @Override
    public void createPermission(PermissionRequestDto requestDto) {
        // 1. 检查权限标识是否存在
        checkPermissionCode(requestDto, null);

        // 2.属性设置
        String permissionId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("permissionId", permissionId);

        Permission permission = new Permission();
        permission.setPermissionName(requestDto.getName());
        permission.setPermissionCode(requestDto.getCode());
        permission.setDescription(requestDto.getDesc());
        permission.setPermissionId(permissionId);
        permission.setResourceId(requestDto.getResourceId());
        permission.setAllowApply(Boolean.TRUE.equals(requestDto.getAllowApply()));
        permission.setAutoApprove(Boolean.TRUE.equals(requestDto.getAutoApprove()));

        // 3. 数据库操作
        super.save(permission);
    }

    /**
     * 获取当前用户权限
     *
     * @return 当前用户权限
     */
    @Cacheable(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, key = "#root.target.generateCurrentUserPermissionsCacheKey()", condition = "#root.target.generateCurrentUserPermissionsCacheCondition()")
    @Override
    public List<PermissionResponseDto> getCurrentUserPermissions() {
        // 1. 获取当前用户
        String userId = AuthUtil.getCurrentUserId();

        if (StringUtils.isNotEmpty(userId)) {
            // 2. 数据库操作
            Page<AuthorizeRecord> pageRequest = new Page<>(1, -1);
            List<String> dynamicUserGroupIds = CommonUtil.stream(userGroupService.getDynamicUserGroups(userId))
                    .map(UserGroup::getUserGroupId).toList();
            getUserPermissions(pageRequest, userId, dynamicUserGroupIds, null, null, null, null, null);
            List<AuthorizeRecord> authorizeRecords = pageRequest.getRecords();
            // 2.1 过滤重复的权限（先按优先级再按授权时间排序）
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
                response.setPermissionId(permission.getPermissionId());
                response.setPermissionName(permission.getPermissionName());
                response.setResourceName(permission.getResource().getResourceName());
                response.setResourceGroupName(permission.getResource().getResourceGroup().getResourceGroupName());
                response.setAuthorizeTime(authorizeRecord.getAuthorizeTime());
                response.setPermissionLocator(generatePermissionLocator(permission));

                // 3.2 限制条件信息
                response.setConditions(CommonUtil.stream(authorizeRecord.getPermissionExps()).map(x -> {
                    PermissionExpResponseDto exp = new PermissionExpResponseDto();
                    exp.setId(x.getExpressionId());
                    return exp;
                }).toList());
                return response;
            }).toList();
        }
        return Collections.emptyList();
    }

    /**
     * 获取用户权限
     *
     * @param page
     *            分页对象
     * @param userId
     *            用户ID
     * @param resourceGroupCode
     *            资源组标识
     * @param resourceGroupNameSearchKeyword
     *            资源组名称搜索关键字
     * @param resourceNameSearchKeyword
     *            资源名称搜索关键字
     * @param permissionNameSearchKeyword
     *            权限名称搜索关键字
     * @param permissionCodeSearchKeyword
     *            权限标识搜索关键字
     */
    @Override
    public void getUserPermissions(IPage<AuthorizeRecord> page,
            String userId,
            List<String> dynamicUserGroupIds,
            String resourceGroupCode,
            String resourceGroupNameSearchKeyword,
            String resourceNameSearchKeyword,
            String permissionNameSearchKeyword,
            String permissionCodeSearchKeyword) {
        permissionRepository.searchUserPermissions(page, userId, dynamicUserGroupIds, resourceGroupCode,
                resourceGroupNameSearchKeyword, resourceNameSearchKeyword, permissionNameSearchKeyword,
                permissionCodeSearchKeyword);
    }

    /**
     * 获取用户组权限
     *
     * @param page
     *            分页对象
     * @param userGroupId
     *            用户组ID
     * @param resourceGroupNameSearchKeyword
     *            资源组名称搜索关键字
     * @param resourceNameSearchKeyword
     *            资源名称搜索关键字
     * @param permissionNameSearchKeyword
     *            权限名称搜索关键字
     * @param permissionCodeSearchKeyword
     *            权限标识搜索关键字
     */
    @Override
    public void getUserGroupPermissions(IPage<AuthorizeRecord> page,
            String userGroupId,
            String resourceGroupNameSearchKeyword,
            String resourceNameSearchKeyword,
            String permissionNameSearchKeyword,
            String permissionCodeSearchKeyword) {
        permissionRepository.searchUserGroupPermissions(page, userGroupId, resourceGroupNameSearchKeyword,
                resourceNameSearchKeyword, permissionNameSearchKeyword, permissionCodeSearchKeyword);
    }

    /**
     * 获取角色权限
     *
     * @param page
     *            分页对象
     * @param roleId
     *            角色ID
     * @param resourceGroupNameSearchKeyword
     *            资源组名称搜索关键字
     * @param resourceNameSearchKeyword
     *            资源名称搜索关键字
     * @param permissionNameSearchKeyword
     *            权限名称搜索关键字
     * @param permissionCodeSearchKeyword
     *            权限标识搜索关键字
     */
    @Override
    public void getRolePermissions(IPage<AuthorizeRecord> page,
            String roleId,
            String resourceGroupNameSearchKeyword,
            String resourceNameSearchKeyword,
            String permissionNameSearchKeyword,
            String permissionCodeSearchKeyword) {
        permissionRepository.searchRolePermissions(page, roleId, resourceGroupNameSearchKeyword,
                resourceNameSearchKeyword, permissionNameSearchKeyword, permissionCodeSearchKeyword);
    }

    /**
     * 获取资源内权限
     *
     * @param page
     *            分页对象
     * @param resourceId
     *            资源ID
     * @param keyword
     *            资源名称 / 标识搜索关键字
     */
    @Override
    public void getResourcePermissions(IPage<Permission> page, String resourceId, String keyword) {
        // 1. 查询数据库
        var query = Wrappers.<Permission>lambdaQuery().eq(Permission::getResourceId, resourceId)
                .orderByAsc(Permission::getPermissionCode);
        if (StringUtils.isNotEmpty(keyword)) {
            query = query
                    .and(o -> o.like(Permission::getPermissionName, keyword)
                            .or(i -> i.like(Permission::getPermissionCode, keyword)))
                    .orderByAsc(Permission::getPermissionCode);
        }
        var permissions = super.list(page, query);

        // 2. 设置权限列表
        page.setRecords(permissions);
    }

    /**
     * 获取权限详情
     *
     * @param permissionId
     *            权限ID
     * @param keyword
     *            被授权主体关键字
     * @return 权限详情
     */
    @Override
    public PermissionResponseDto detail(String permissionId, String keyword) {
        PermissionResponseDto permissionResponse = new PermissionResponseDto();
        // 1. 查询数据库
        Permission permission = permissionRepository.getPermission(permissionId);
        if (permission == null || permission.getResource() == null) {
            return permissionResponse;
        }

        // 2. 设置基本信息
        permissionResponse.setPermissionId(permission.getPermissionId());
        permissionResponse.setPermissionName(permission.getPermissionName());
        permissionResponse.setPermissionCode(permission.getPermissionCode());
        permissionResponse.setPermissionDesc(permission.getDescription());
        permissionResponse.setResourceId(permission.getResource().getResourceId());
        permissionResponse.setAllowApply(permission.getAllowApply());
        permissionResponse.setAutoApprove(permission.getAutoApprove());

        permissionResponse.setPermissionLocator(generatePermissionLocator(permission));

        // 3. 设置授权记录
        var records = CommonUtil.stream(permissionRepository.searchPermissionAuthorizeRecords(permissionId, keyword))
                .map(authorizeRecord -> {
                    AuthorizeRecordResponseDto authorizeRecordResponse = new AuthorizeRecordResponseDto();
                    authorizeRecordResponse.setAuthorizeId(authorizeRecord.getAuthorizeId());
                    authorizeRecordResponse.setPrincipalType(authorizeRecord.getType());
                    authorizeRecordResponse.setAuthorizeTime(authorizeRecord.getAuthorizeTime());
                    authorizeRecordResponse.setPriority(authorizeRecord.getPriority());
                    authorizeRecordResponse.setAuthorizerId(authorizeRecord.getAuthorizerId());
                    authorizeRecordResponse.setAuthorizerUsername(authorizeRecord.getAuthorizerUsername());

                    AuthorizeTypeEnum authorizeType = AuthorizeTypeEnum.fromType(authorizeRecord.getType());
                    if (Objects.nonNull(authorizeType)) {
                        authorizeRecordResponse.setAuthorizeType(authorizeType.getDisplayName());
                    }

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
     * @param resourceIds
     *            资源ID集合
     */
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
            List<AuthorizeRecord> authorizeRecords = authorizeService
                    .list(Wrappers.<AuthorizeRecord>lambdaQuery().in(AuthorizeRecord::getPermissionId, ids));
            if (CollectionUtils.isNotEmpty(authorizeRecords)) {
                return;
            }
            authorizeService.removeAuthorization(
                    CommonUtil.stream(authorizeRecords).map(AuthorizeRecord::getAuthorizeId).toList());

            // 4. 清除用户权限缓存
            for (AuthorizeRecord authorizeRecord : authorizeRecords) {
                this.clearUserPermissionsCacheByAuthorizeId(authorizeRecord.getAuthorizeId());
            }
        }
    }

    /**
     * 删除权限
     *
     * @param permissionId
     *            权限ID
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION, sysOperation = SysOperationType.DELETE, success = "删除了权限（{{ @linkGen.toLink(#permissionId, T(ResourceType).PERMISSION) }}）", fail = "删除权限（{{ @linkGen.toLink(#permissionId, T(ResourceType).PERMISSION) }}）失败")
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
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION, sysOperation = SysOperationType.UPDATE, success = "修改了权限（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).PERMISSION) }}）", fail = "修改权限（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).PERMISSION) }}）失败")
    @Transactional
    @Override
    public void updatePermission(PermissionRequestDto requestDto) {
        String permissionId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        var rawPermission = super.getById(requestDto.getId());
        if (Objects.isNull(rawPermission)) {
            return;
        }
        compareObjBuilder.id(permissionId);
        compareObjBuilder.before(rawPermission);

        // 2. 检查权限标识是否存在
        checkPermissionCode(requestDto, rawPermission);

        // 3. 属性设置
        Permission updatePermission = new Permission();
        updatePermission.setPermissionId(requestDto.getId());
        updatePermission.setPermissionName(requestDto.getName());
        updatePermission.setPermissionCode(requestDto.getCode());
        updatePermission.setDescription(requestDto.getDesc());
        updatePermission.setVersion(rawPermission.getVersion());
        CommonUtil.callSetWithCheck(Objects::nonNull, updatePermission::setAllowApply, requestDto::getAllowApply);
        CommonUtil.callSetWithCheck(Objects::nonNull, updatePermission::setAutoApprove, requestDto::getAutoApprove);

        // 4. 数据库操作
        super.updateById(updatePermission);

        // 5. 清除用户权限缓存
        List<AuthorizeRecord> authorizeRecords = authorizeService
                .list(Wrappers.<AuthorizeRecord>lambdaQuery().eq(AuthorizeRecord::getPermissionId, permissionId));
        if (CollectionUtils.isNotEmpty(authorizeRecords)) {
            for (AuthorizeRecord authorizeRecord : authorizeRecords) {
                this.clearUserPermissionsCacheByAuthorizeId(authorizeRecord.getAuthorizeId());
            }
        }

        compareObjBuilder.after(super.getById(permissionId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 获取权限表达式关联的权限
     *
     * @param expressionId
     *            表达式ID
     * @return 权限表达式关联的授权
     */
    @Override
    public List<AuthorizeRecord> getExpPermissions(String expressionId) {
        return permissionRepository.searchExpPermission(expressionId);
    }

    /**
     * 校验权限
     *
     * @param requestDto
     *            校验权限请求
     * @return 校验权限响应
     */
    @Override
    public List<VerifyPermissionResponseDto> verifyPermissions(VerifyPermissionsRequestDto requestDto) {
        // 1. 获取当前用户权限
        PermissionService proxyService = (PermissionService) AopContext.currentProxy();
        Map<String, PermissionResponseDto> permissions = CommonUtil.stream(proxyService.getCurrentUserPermissions())
                .collect(Collectors.toMap(PermissionResponseDto::getPermissionLocator, p -> p));

        // 2. 校验权限
        return CommonUtil.stream(requestDto.getPermissions())
                .map(permissionLocator -> {
                    var responseBuilder = VerifyPermissionResponseDto.builder();
                    responseBuilder.permission(permissionLocator);

                    // 2.1 检查是否存在该权限
                    if (!permissions.containsKey(permissionLocator)) {
                        responseBuilder.allow(false);
                        return responseBuilder.build();
                    }

                    // 2.2 检查是否存在限制条件
                    List<PermissionExpResponseDto> conditions = permissions.get(permissionLocator).getConditions();
                    if (CollectionUtils.isNotEmpty(conditions)) {
                        // 2.2.1 执行表达式
                        responseBuilder.allow(true);
                        for (PermissionExpResponseDto condition : conditions) {
                            if (!Boolean.TRUE.equals(permissionExpService.executePermissionExp(condition.getId(),
                                    requestDto.getContext()))) {
                                responseBuilder.allow(false);
                                break;
                            }
                        }
                    } else {
                        responseBuilder.allow(true);
                    }
                    return responseBuilder.build();
                }).toList();
    }

    /**
     * 获取可申请的权限树
     *
     * @return 权限树（按资源组 -> 资源 -> 权限 三层结构）
     */
    @Override
    public List<PermissionTreeNodeResponseDto> getAvailablePermissionTree() {
        // 1. 获取可申请的权限
        List<Permission> availablePermissions = permissionRepository.getAllowApplyPermissions();
        if (CollectionUtils.isEmpty(availablePermissions)) {
            return Collections.emptyList();
        }

        // 2. 获取当前用户权限
        PermissionService proxyService = (PermissionService) AopContext.currentProxy();
        final List<String> ownedPermissionIds = new ArrayList<>(CommonUtil
                .stream(proxyService.getCurrentUserPermissions()).map(PermissionResponseDto::getPermissionId).toList());

        // 3. 获取用户申请中的权限
        final List<String> pendingPermissionIds = new ArrayList<>();
        List<PermissionRequest> permissionRequestList = permissionRequestService
                .list(Wrappers.<PermissionRequest>lambdaQuery()
                        .select(PermissionRequest::getRequestId)
                        .eq(PermissionRequest::getUserId, AuthUtil.getCurrentUserId()));
        if (CollectionUtils.isNotEmpty(permissionRequestList)) {
            pendingPermissionIds
                    .addAll(CommonUtil
                            .stream(permissionRequestItemService.list(Wrappers.<PermissionRequestItem>lambdaQuery()
                                    .in(PermissionRequestItem::getRequestId,
                                            permissionRequestList.stream().map(PermissionRequest::getRequestId)
                                                    .toList())
                                    .eq(PermissionRequestItem::getStatus, PermissionRequestStatusEnum.PENDING.name())))
                            .map(PermissionRequestItem::getPermissionId).toList());
        }

        List<Resource> availableResource = availablePermissions.stream().map(Permission::getResource).distinct()
                .toList();
        List<ResourceGroup> availableResourceGroup = availableResource.stream().map(Resource::getResourceGroup)
                .distinct().toList();
        Map<String, List<Resource>> groupedAvailableResource = CommonUtil.stream(availableResource)
                .collect(Collectors.groupingBy(r -> r.getResourceGroup().getResourceGroupId()));
        Map<String, List<Permission>> groupedAvailablePermissions = CommonUtil.stream(availablePermissions)
                .collect(Collectors.groupingBy(p -> p.getResource().getResourceId()));

        // 4. 构建权限树
        return CommonUtil.stream(availableResourceGroup).map(rg -> {
            // 4.1 资源组
            PermissionTreeNodeResponseDto treeNode1 = new PermissionTreeNodeResponseDto();
            treeNode1.setId(rg.getResourceGroupId());
            treeNode1.setName(rg.getResourceGroupName());
            treeNode1.setCode(rg.getResourceGroupCode());
            treeNode1.setType("RESOURCE_GROUP");

            // 4.2 资源
            treeNode1.setChildren(CommonUtil.stream(groupedAvailableResource.get(rg.getResourceGroupId())).map(r -> {
                PermissionTreeNodeResponseDto treeNode2 = new PermissionTreeNodeResponseDto();
                treeNode2.setId(r.getResourceId());
                treeNode2.setName(r.getResourceName());
                treeNode2.setCode(r.getResourceCode());
                treeNode2.setType("RESOURCE");

                // 4.3 权限
                treeNode2.setChildren(CommonUtil.stream(groupedAvailablePermissions.get(r.getResourceId())).map(p -> {
                    PermissionTreeNodeResponseDto treeNode3 = new PermissionTreeNodeResponseDto();
                    treeNode3.setId(p.getPermissionId());
                    treeNode3.setName(p.getPermissionName());
                    treeNode3.setCode(p.getPermissionCode());
                    treeNode3.setAutoApprove(p.getAutoApprove());
                    treeNode3.setPending(pendingPermissionIds.contains(p.getPermissionId()));
                    treeNode3.setOwned(ownedPermissionIds.contains(p.getPermissionId()));
                    treeNode3.setType("PERMISSION");

                    return treeNode3;
                }).toList());

                return treeNode2;
            }).toList());

            return treeNode1;
        }).toList();
    }

    /**
     * 清除用户权限缓存
     *
     * @param roleId
     *            角色 ID
     */
    @Override
    public void clearUserPermissionsCacheByRoleId(String roleId) {
        // 1. 获取角色下的所有用户 ID
        List<RoleResponseDto> rolePrincipals = roleService.getRolePrincipals(1, -1, roleId, null).getList();
        List<String> userIds = new ArrayList<>();
        for (RoleResponseDto rolePrincipal : rolePrincipals) {
            if (PrincipalTypeEnum.USER.getType().equals(rolePrincipal.getPrincipalId())) {
                userIds.add(rolePrincipal.getPrincipalId());
            }

            if (PrincipalTypeEnum.USER_GROUP.getType().equals(rolePrincipal.getPrincipalId())) {
                userIds.addAll(
                        CommonUtil
                                .stream(userGroupService.getGroupUsers(1, -1, rolePrincipal.getPrincipalId(), null)
                                        .getList())
                                .map(UserResponseDto::getId).toList());
            }
        }

        // 2. 构建 Redis 缓存 key
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        List<String> cacheKeys = CommonUtil.stream(userIds).map(id -> CacheConstants.CACHE_CURRENT_USER_PERMISSIONS
                + "::" + tenantCode + CommonConstants.COLON + id).toList();

        // 3. 清除缓存
        if (CollectionUtils.isNotEmpty(cacheKeys)) {
            RedisUtil.delete(cacheKeys.toArray(new String[0]));
        }
    }

    /**
     * 清除用户权限缓存
     *
     * @param userId
     *            用户 ID
     */
    @Override
    public void clearUserPermissionsCacheByUserId(String userId) {
        // 1. 清除缓存
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        RedisUtil.delete(
                CacheConstants.CACHE_CURRENT_USER_PERMISSIONS + "::" + tenantCode + CommonConstants.COLON + userId);
    }

    /**
     * 清除用户组权限缓存
     *
     * @param groupId
     *            用户组 ID
     */
    @Override
    public void clearUserPermissionsCacheByUserGroupId(String groupId) {
        // 1. 获取用户组下的所有用户 ID
        List<String> userIds = CommonUtil.stream(userGroupService.getGroupUsers(1, -1, groupId, null).getList())
                .map(UserResponseDto::getId).toList();

        // 2. 构建 Redis 缓存 key
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        List<String> cacheKeys = CommonUtil.stream(userIds).map(id -> CacheConstants.CACHE_CURRENT_USER_PERMISSIONS
                + "::" + tenantCode + CommonConstants.COLON + id).toList();

        // 3. 清除缓存
        if (CollectionUtils.isNotEmpty(cacheKeys)) {
            RedisUtil.delete(cacheKeys.toArray(new String[0]));
        }
    }

    /**
     * 清除用户权限缓存
     *
     * @param authorizeId
     *            授权 ID
     */
    @Override
    public void clearUserPermissionsCacheByAuthorizeId(String authorizeId) {
        // 1. 获取授权记录
        AuthorizeRecord authorizeRecord = authorizeService.getById(authorizeId);
        if (Objects.isNull(authorizeRecord)) {
            return;
        }

        // 2. 清除角色权限
        if (Objects.nonNull(authorizeRecord.getRoleId())) {
            clearUserPermissionsCacheByRoleId(authorizeRecord.getRoleId());
        }

        // 3. 清除用户组权限
        if (Objects.nonNull(authorizeRecord.getUserGroupId())) {
            clearUserPermissionsCacheByUserGroupId(authorizeRecord.getUserGroupId());
        }

        // 4. 清除用户权限
        if (Objects.nonNull(authorizeRecord.getUserId())) {
            clearUserPermissionsCacheByUserId(authorizeRecord.getUserId());
        }
    }

    private Tuple4<String, String, String, String> getPrincipal(AuthorizeRecord authorizeRecord) {
        User user = authorizeRecord.getUser();
        UserGroup userGroup = authorizeRecord.getUserGroup();
        Role role = authorizeRecord.getRole();

        if (user != null) {
            return Tuple.of(user.getUserId(), user.getUsername(), PrincipalTypeEnum.USER.getType(),
                    PrincipalTypeEnum.USER.getDisplayName());
        }

        if (userGroup != null) {
            return Tuple.of(userGroup.getUserGroupId(), userGroup.getUserGroupName(),
                    PrincipalTypeEnum.USER_GROUP.getType(), PrincipalTypeEnum.USER_GROUP.getDisplayName());
        }

        if (role != null) {
            return Tuple.of(role.getRoleId(), role.getRoleName(), PrincipalTypeEnum.ROLE.getType(),
                    PrincipalTypeEnum.ROLE.getDisplayName());
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
        return TenantContextHolder.getTenantContext().getTenantCode() + ":" + userId;
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

    private void checkPermissionCode(PermissionRequestDto requestDto, Permission rawPermission) {
        if (Objects.nonNull(rawPermission)
                && Strings.CS.equals(requestDto.getCode(), rawPermission.getPermissionCode())) {
            return;
        }

        if (Objects.isNull(rawPermission) && Objects.isNull(resourceService.getById(requestDto.getResourceId()))) {
            throw new BizException(MessageConstants.PERMISSION_MSG_1001);
        }

        if (Objects.nonNull(
                super.getOne(Wrappers.<Permission>lambdaQuery().eq(Permission::getPermissionCode, requestDto.getCode())
                        .and(q -> q.eq(Permission::getResourceId, requestDto.getResourceId()))))) {
            throw new BizException(MessageConstants.PERMISSION_MSG_1000, requestDto.getCode());
        }
    }

    private String generatePermissionLocator(Permission permission) {
        if (Objects.isNull(permission)) {
            return StringUtils.EMPTY;
        }

        var resource = permission.getResource();
        var resourceGroup = resource != null ? resource.getResourceGroup() : null;

        List<String> locatorParts = List.of(
                resourceGroup != null ? resourceGroup.getResourceGroupCode() : StringUtils.EMPTY,
                resource != null ? resource.getResourceCode() : StringUtils.EMPTY,
                permission.getPermissionCode());
        return CommonUtil.stream(locatorParts).map(x -> {
            if (Objects.isNull(x)) {
                return StringUtils.EMPTY;
            }
            return x.trim();
        }).collect(Collectors.joining(CommonConstants.COLON));
    }
}
