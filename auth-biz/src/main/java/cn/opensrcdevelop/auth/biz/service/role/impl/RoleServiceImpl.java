package cn.opensrcdevelop.auth.biz.service.role.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.PrincipalTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.role.RoleMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.role.RoleRequestDto;
import cn.opensrcdevelop.auth.biz.dto.role.RoleResponseDto;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.role.Role;
import cn.opensrcdevelop.auth.biz.entity.role.RoleMapping;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroup;
import cn.opensrcdevelop.auth.biz.mapper.role.RoleMapper;
import cn.opensrcdevelop.auth.biz.mapper.role.RoleMappingMapper;
import cn.opensrcdevelop.auth.biz.repository.role.RoleRepository;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.permission.PermissionService;
import cn.opensrcdevelop.auth.biz.service.role.RoleMappingService;
import cn.opensrcdevelop.auth.biz.service.role.RoleService;
import cn.opensrcdevelop.auth.biz.service.user.group.UserGroupService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.SqlHelper;
import io.vavr.Tuple;
import io.vavr.Tuple4;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RoleServiceImpl extends ServiceImpl<RoleMapper, Role> implements RoleService {

    private final RoleMappingService roleMappingService;
    private final RoleRepository roleRepository;
    private final PermissionService permissionService;
    private final AuthorizeService authorizeService;

    @Resource
    @Lazy
    private UserGroupService userGroupService;

    /**
     * 创建角色
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.ROLE,
            sysOperation = SysOperationType.CREATE,
            success = "创建了角色（{{ @linkGen.toLink(#roleId, T(ResourceType).ROLE) }}）",
            fail = "创建角色（{{ #requestDto.name }}）失败"
    )
    @Transactional
    @Override
    public void createRole(RoleRequestDto requestDto) {
        // 1. 检查角色标识是否存在
        checkRoleCode(requestDto, null);

        // 2. 设置属性
        String roleId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("roleId", roleId);

        Role role = new Role();
        role.setRoleId(CommonUtil.getUUIDV7String());
        role.setRoleName(requestDto.getName());
        role.setRoleCode(requestDto.getCode());
        role.setDescription(requestDto.getDesc());

        // 3. 数据库操作
        super.save(role);
    }

    /**
     * 创建角色映射
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.ROLE,
            sysOperation = SysOperationType.CREATE,
            success = "为用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）、" +
                    "用户组（{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}）添加了角色（" +
                    "{{ @linkGen.toLink(#requestDto.roleIds, T(ResourceType).ROLE) }}）",
            fail = "为用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）、" +
                    "用户组（{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}）添加角色（" +
                    "{{ @linkGen.toLink(#requestDto.roleIds, T(ResourceType).ROLE) }}）失败"
    )
    @Transactional
    @Override
    public void createUserRoleMapping(RoleMappingRequestDto requestDto) {

        // 1. 创建映射关系
        List<RoleMapping> mappings = getMappings(requestDto);

        if (CollectionUtils.isNotEmpty(mappings)) {
            // 2. 删除待创建的映射关系，避免重复创建
            SqlHelper.executeBatch(getSqlSessionFactory(), this.log, mappings, mappings.size(), (sqlSession, roleMapping) -> {
                RoleMappingMapper mapper = sqlSession.getMapper(RoleMappingMapper.class);
                String principalId = roleMapping.getUserId() == null ? roleMapping.getUserGroupId() : roleMapping.getUserId();
                mapper.delete(Wrappers.<RoleMapping>lambdaQuery().eq(RoleMapping::getRoleId, roleMapping.getRoleId())
                        .and(o -> o.eq(RoleMapping::getUserId, principalId).or(i -> i.eq(RoleMapping::getUserGroupId, principalId)))
                );
            });

            // 3. 数据库操作
            roleMappingService.saveBatch(mappings);
        }
    }

    /**
     * 获取用户角色
     *
     * @param userId 用户ID
     */
    @Override
    public List<Role> getUserRoles(String userId) {
        List<String> dynamicUserGroupIds = CommonUtil.stream(userGroupService.getDynamicUserGroups(userId)).map(UserGroup::getUserGroupId).collect(Collectors.toList());
        return CommonUtil.stream(roleRepository.searchUserRoles(userId, dynamicUserGroupIds)).map(RoleMapping::getRole).toList();
    }

    /**
     * 移除角色映射
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.ROLE,
            sysOperation = SysOperationType.DELETE,
            success = "删除了用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）、" +
                    "用户组（{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}）的角色（" +
                    "{{ @linkGen.toLink(#requestDto.roleIds, T(ResourceType).ROLE) }}）",
            fail = "删除用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）、" +
                    "用户组（{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}）的角色（" +
                    "{{ @linkGen.toLink(#requestDto.roleIds, T(ResourceType).ROLE) }}）失败"
    )
    @Transactional
    @Override
    public void removeUserRoleMapping(RoleMappingRequestDto requestDto) {

        // 1. 待删除的映射关系
        List<RoleMapping> deleteTargetMappings = getMappings(requestDto);

        if (CollectionUtils.isNotEmpty(deleteTargetMappings)) {
            // 2. 数据库操作
            SqlHelper.executeBatch(getSqlSessionFactory(), this.log, deleteTargetMappings, deleteTargetMappings.size(), (sqlSession, roleMapping) -> {
                RoleMappingMapper mapper = sqlSession.getMapper(RoleMappingMapper.class);
                String principalId = roleMapping.getUserId() == null ? roleMapping.getUserGroupId() : roleMapping.getUserId();
                mapper.delete(Wrappers.<RoleMapping>lambdaQuery().eq(RoleMapping::getRoleId, roleMapping.getRoleId())
                        .and(o -> o.eq(RoleMapping::getUserId, principalId).or(i -> i.eq(RoleMapping::getUserGroupId, principalId)))
                );
            });
        }
    }

    /**
     * 获取角色列表
     *
     * @param page    页数
     * @param size    条数
     * @param keyword 角色名称 / 标识检索关键字
     * @return 所有角色
     */
    @Override
    public PageData<RoleResponseDto> listRoles(int page, int size, String keyword) {
        // 1. 数据库操作
        Page<Role> pageRequest = new Page<>(page, size);
        List<Role> roles;
        if (StringUtils.isNotEmpty(keyword)) {
            roles = super.list(pageRequest, Wrappers.<Role>lambdaQuery().like(Role::getRoleName, keyword).or(o -> o.like(Role::getRoleCode, keyword)).orderByAsc(Role::getRoleCode));
        } else {
            roles = super.list(pageRequest, Wrappers.<Role>lambdaQuery().orderByAsc(Role::getRoleCode));
        }

        // 2. 属性设置
        PageData<RoleResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var records = CommonUtil.stream(roles).map(role -> {
            RoleResponseDto roleResponse = new RoleResponseDto();
            roleResponse.setId(role.getRoleId());
            roleResponse.setName(role.getRoleName());
            roleResponse.setCode(role.getRoleCode());
            return roleResponse;
        }).toList();
        pageData.setList(records);
        return pageData;
    }

    /**
     * 删除指定用户 / 用户组的全部角色映射关系
     *
     * @param principalId 用户 / 用户组ID
     */
    @Transactional
    @Override
    public void removeUserRoleMapping(String principalId) {
        roleMappingService.remove(Wrappers.<RoleMapping>lambdaQuery().eq(RoleMapping::getUserId, principalId).or(o -> o.eq(RoleMapping::getUserGroupId, principalId)));
    }

    /**
     * 获取角色主体
     *
     * @param page    页数
     * @param size    条数
     * @param roleId  角色ID
     * @param keyword 用户名 / 用户组名称关键字
     * @return 角色主体
     */
    @Override
    public PageData<RoleResponseDto> getRolePrincipals(int page, int size, String roleId, String keyword) {
        // 1. 查询数据库
        Page<RoleMapping> pageRequest = new Page<>(page, size);
        roleRepository.searchRolePrincipals(pageRequest, roleId, keyword);

        // 2. 属性设置
        PageData<RoleResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setPages(pageRequest.getPages());
        pageData.setSize(pageRequest.getSize());

        var roles = CommonUtil.stream(pageRequest.getRecords()).map(roleMapping -> {
            RoleResponseDto roleResponse = new RoleResponseDto();

            // 2.1 角色主体设置
            var principal = getRolePrincipal(roleMapping);
            roleResponse.setPrincipalId(principal._1);
            roleResponse.setPrincipal(principal._2);
            roleResponse.setPrincipalType(principal._3);
            roleResponse.setPrincipalTypeDisplayName(principal._4);
            return roleResponse;
        }).toList();
        pageData.setList(roles);

        return pageData;
    }

    /**
     * 获取角色详情
     *
     * @param roleId 角色ID
     * @return 角色详情
     */
    @Override
    public RoleResponseDto detail(String roleId) {
        RoleResponseDto roleResponseDto = new RoleResponseDto();
        // 1. 查询数据库
        Role role = super.getById(roleId);
        if (role == null) {
            return roleResponseDto;
        }

        // 2. 属性设置
        roleResponseDto.setId(role.getRoleId());
        roleResponseDto.setName(role.getRoleName());
        roleResponseDto.setCode(role.getRoleCode());
        roleResponseDto.setDesc(role.getDescription());
        return roleResponseDto;
    }

    /**
     * 更新角色
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.ROLE,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了角色（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).ROLE) }}）",
            fail = "修改角色（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).ROLE) }}）失败"
    )
    @Transactional
    @Override
    public void updateRole(RoleRequestDto requestDto) {
        String roleId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        var rawRole = super.getById(requestDto.getId());
        if (Objects.isNull(rawRole)) {
            return;
        }
        compareObjBuilder.id(roleId);
        compareObjBuilder.before(rawRole);

        // 2. 检查角色标识是否存在
        checkRoleCode(requestDto, rawRole);

        // 3. 属性编辑
        Role updateRole = new Role();
        updateRole.setRoleId(requestDto.getId());
        updateRole.setRoleName(requestDto.getName());
        updateRole.setRoleCode(requestDto.getCode());
        updateRole.setDescription(requestDto.getDesc());
        updateRole.setVersion(rawRole.getVersion());

        // 4. 数据库操作
        super.updateById(updateRole);

        compareObjBuilder.after(super.getById(roleId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 删除角色
     *
     * @param roleId 角色ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.ROLE,
            sysOperation = SysOperationType.DELETE,
            success = "删除了角色（{{ @linkGen.toLink(#roleId, T(ResourceType).ROLE) }}）",
            fail = "删除角色（{{ @linkGen.toLink(#roleId, T(ResourceType).ROLE) }}）失败"
    )
    @Transactional
    @Override
    public void removeRole(String roleId) {
        // 1. 删除角色
        super.removeById(roleId);

        // 2. 删除关联的全部角色映射关系
        roleMappingService.remove(Wrappers.<RoleMapping>lambdaQuery().eq(RoleMapping::getRoleId, roleId));

        // 3. 删除关联的授权
        authorizeService.removeAuthorization(roleId);
    }

    /**
     * 获取权限
     *
     * @param page                           页数
     * @param size                           条数
     * @param roleId                         角色ID
     * @param resourceGroupNameSearchKeyword 资源组名称搜索关键字
     * @param resourceNameSearchKeyword      资源名称搜索关键字
     * @param permissionNameSearchKeyword    权限名称搜索关键字
     * @param permissionCodeSearchKeyword    权限标识搜索关键字
     * @return 权限信息
     */
    @Override
    public PageData<PermissionResponseDto> getPermissions(int page, int size, String roleId, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword) {
        // 1. 查询数据库
        Page<AuthorizeRecord> pageRequest = new Page<>(page, size);
        permissionService.getRolePermissions(pageRequest, roleId, resourceGroupNameSearchKeyword, resourceNameSearchKeyword, permissionNameSearchKeyword, permissionCodeSearchKeyword);

        // 2. 属性编辑
        PageData<PermissionResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        List<PermissionResponseDto> permissionResponseList = CommonUtil.stream(pageRequest.getRecords()).map(authorizeRecord -> {
            PermissionResponseDto permissionResponse = new PermissionResponseDto();

            // 2.1 权限响应属性
            var permission = authorizeRecord.getPermission();
            permissionResponse.setAuthorizeId(authorizeRecord.getAuthorizeId());
            permissionResponse.setPriority(authorizeRecord.getPriority());
            permissionResponse.setPermissionId(permission.getPermissionId());
            permissionResponse.setPermissionName(permission.getPermissionName());
            permissionResponse.setPermissionCode(permission.getPermissionCode());
            permissionResponse.setResourceId(permission.getResource().getResourceId());
            permissionResponse.setResourceCode(permission.getResource().getResourceCode());
            permissionResponse.setResourceName(permission.getResource().getResourceName());
            permissionResponse.setResourceGroupId(permission.getResource().getResourceGroup().getResourceGroupId());
            permissionResponse.setResourceGroupCode(permission.getResource().getResourceGroup().getResourceGroupCode());
            permissionResponse.setResourceGroupName(permission.getResource().getResourceGroup().getResourceGroupName());

            // 2.2 限定条件
            var conditions = CommonUtil.stream(authorizeRecord.getPermissionExps()).map(exp -> {
                PermissionExpResponseDto condition = new PermissionExpResponseDto();
                condition.setId(exp.getExpressionId());
                condition.setName(exp.getExpressionName());
                condition.setDesc(exp.getDescription());
                return condition;
            }).toList();
            permissionResponse.setConditions(conditions);

            return permissionResponse;
        }).toList();
        pageData.setList(permissionResponseList);
        return pageData;
    }

    private List<RoleMapping> getMappings(RoleMappingRequestDto requestDto) {
        var userIds = CommonUtil.stream(requestDto.getUserIds()).filter(StringUtils::isNotBlank).collect(Collectors.toSet());
        var userGroupIds = CommonUtil.stream(requestDto.getUserGroupIds()).filter(StringUtils::isNotBlank).toList();
        var roleIds = CommonUtil.stream(requestDto.getRoleIds()).filter(StringUtils::isNotBlank).collect(Collectors.toSet());

        List<RoleMapping> mappings = new ArrayList<>();

        if (CollectionUtils.isNotEmpty(roleIds)) {
            if (CollectionUtils.isNotEmpty(userIds)) {
                userIds.forEach(userId -> roleIds.forEach((roleId -> {
                    RoleMapping mapping = new RoleMapping();
                    mapping.setUserId(userId);
                    mapping.setRoleId(roleId);
                    mappings.add(mapping);
                })));
            }

            if (CollectionUtils.isNotEmpty(userGroupIds)) {
                userGroupIds.forEach(userGroupId -> roleIds.forEach((roleId -> {
                    RoleMapping mapping = new RoleMapping();
                    mapping.setUserGroupId(userGroupId);
                    mapping.setRoleId(roleId);
                    mappings.add(mapping);
                })));
            }
        }
        return mappings;
    }

    private Tuple4<String, String, String, String> getRolePrincipal(RoleMapping roleMapping) {
        User user = roleMapping.getUser();
        UserGroup userGroup = roleMapping.getUserGroup();

        if (user != null) {
            return Tuple.of(user.getUserId(), user.getUsername(), PrincipalTypeEnum.USER.getType(), PrincipalTypeEnum.USER.getDisplayName());
        }

        if (userGroup != null) {
            return Tuple.of(userGroup.getUserGroupId(), userGroup.getUserGroupName(), PrincipalTypeEnum.USER_GROUP.getType(), PrincipalTypeEnum.USER_GROUP.getDisplayName());
        }

        return Tuple.of(null, null, null, null);
    }

    private void checkRoleCode(RoleRequestDto requestDto, Role rawRole) {
        if (Objects.nonNull(rawRole) && StringUtils.equals(requestDto.getCode(), rawRole.getRoleCode())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<Role>lambdaQuery().eq(Role::getRoleCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.ROLE_MSG_1000, requestDto.getCode());
        }
    }
}
