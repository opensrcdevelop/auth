package cn.opensrcdevelop.auth.biz.service.user.group.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.ConjunctionType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.UserGroupType;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.dto.user.UserResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.group.DynamicUserGroupConditionsDto;
import cn.opensrcdevelop.auth.biz.dto.user.group.UserGroupMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.group.UserGroupRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.group.UserGroupResponseDto;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroup;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroupMapping;
import cn.opensrcdevelop.auth.biz.mapper.user.group.UserGroupMapper;
import cn.opensrcdevelop.auth.biz.mapper.user.group.UserGroupMappingMapper;
import cn.opensrcdevelop.auth.biz.repository.user.UserRepository;
import cn.opensrcdevelop.auth.biz.repository.user.group.UserGroupRepository;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.permission.PermissionService;
import cn.opensrcdevelop.auth.biz.service.role.RoleService;
import cn.opensrcdevelop.auth.biz.service.user.group.UserGroupMappingService;
import cn.opensrcdevelop.auth.biz.service.user.group.UserGroupService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.SqlHelper;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class UserGroupServiceImpl extends ServiceImpl<UserGroupMapper, UserGroup> implements UserGroupService {

    private final UserGroupMappingService userGroupMappingService;
    private final UserGroupRepository userGroupRepository;
    private final RoleService roleService;
    private final AuthorizeService authorizeService;
    private final PermissionService permissionService;
    private final UserRepository userRepository;

    /**
     * 创建用户组
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.USER_GROUP,
            sysOperation = SysOperationType.CREATE,
            success = "创建了用户组（{{ @linkGen.toLink(#userGroupId, T(ResourceType).USER_GROUP) }}）",
            fail = "创建用户组（{{ #requestDto.name }}）失败"
    )
    @Transactional
    @Override
    public void createUserGroup(UserGroupRequestDto requestDto) {
        // 1. 动态用户组参数校验
        if (UserGroupType.DYNAMIC == requestDto.getType()) {
            CommonUtil.validateBean(requestDto, UserGroupRequestDto.DynamicUserGroup.class);
        }

        // 2. 检查用户标识是否存在
        checkUserGroupCode(requestDto, null);

        // 3. 属性设置
        String userGroupId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("userGroupId", userGroupId);

        UserGroup userGroup = new UserGroup();
        userGroup.setUserGroupId(userGroupId);
        userGroup.setUserGroupName(requestDto.getName());
        userGroup.setUserGroupCode(requestDto.getCode());
        userGroup.setDescription(requestDto.getDesc());
        userGroup.setUserGroupType(requestDto.getType().name());

        if (UserGroupType.DYNAMIC.equals(requestDto.getType())) {
            checkDynamicUserGroupConditions(requestDto);
            userGroup.setDynamicConditions(CommonUtil.nonJdkSerializeObject(requestDto.getConditions()));
        }

        // 4. 数据库操作
        super.save(userGroup);
    }

    /**
     * 创建用户组映射
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.USER_GROUP,
            sysOperation = SysOperationType.CREATE,
            success = "向用户组（{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}" +
                    "）中添加了用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）",
            fail = "向用户组（{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}" +
                    "）中添加用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）失败"
    )
    @Transactional
    @Override
    public void createUserGroupMapping(UserGroupMappingRequestDto requestDto) {

        // 1. 创建映射关系
        List<UserGroupMapping> mappings = getMappings(requestDto);

        if (CollectionUtils.isNotEmpty(mappings)) {
            // 2. 删除待创建的映射关系，避免重复创建
            SqlHelper.executeBatch(getSqlSessionFactory(), this.log, mappings, mappings.size(), ((sqlSession, userUserGroupMapping) -> {
                UserGroupMappingMapper mapper = sqlSession.getMapper(UserGroupMappingMapper.class);
                mapper.delete(Wrappers.<UserGroupMapping>lambdaQuery().eq(UserGroupMapping::getUserId, userUserGroupMapping.getUserId()).and(o -> o.eq(UserGroupMapping::getUserGroupId, userUserGroupMapping.getUserGroupId())));
            }));

            // 3. 数据库操作
            userGroupMappingService.saveBatch(mappings);
        }
    }

    /**
     * 获取用户所属的静态用户组信息
     *
     * @param userId 用户 ID
     * @return 静态用户组信息
     */
    @Override
    public List<UserGroup> getUserGroups(String userId) {
        return userGroupRepository.searchUserGroups(userId);
    }

    /**
     * 获取用户所属的动态用户组信息
     *
     * @param userId 用户 ID
     * @return 动态用户组信息
     */
    @Override
    public List<UserGroup> getDynamicUserGroups(String userId) {
        // 1. 获取全部用户组
        List<UserGroup> userGroups = super.list(Wrappers.<UserGroup>lambdaQuery().eq(UserGroup::getUserGroupType, UserGroupType.DYNAMIC.name()));

        // 2. 获取各个动态用户组的用户
        List<CompletableFuture<Void>> allFutures = new ArrayList<>();
        for (UserGroup userGroup : userGroups) {
            allFutures.add(CompletableFuture.runAsync(() -> {
                QueryWrapper<User> queryWrapper = new QueryWrapper<>();
                queryWrapper.eq(CommonUtil.extractFieldNameFromGetter(User::getDeleted), false);
                queryWrapper.and(q -> editQueryWrapper(q, CommonUtil.nonJdkDeserializeObject(
                        userGroup.getDynamicConditions(),
                        DynamicUserGroupConditionsDto.class
                )));
                userGroup.setUsers(userRepository.searchUsers(queryWrapper, 0, 0));
            }, SpringContextUtil.getBean(ExecutorConstants.EXECUTOR_IO_DENSE)));
        }
        CompletableFuture.allOf(allFutures.toArray(new CompletableFuture[0])).join();


        // 3. 过滤用户所属的动态用户组
        return CommonUtil.stream(userGroups)
                .filter(userGroup -> CommonUtil.stream(userGroup.getUsers())
                        .anyMatch(user -> user.getUserId().equals(userId)))
                .peek(userGroup -> userGroup.setUsers(null))
                .toList();
    }

    /**
     * 获取用户组列表
     *
     * @param page    页数
     * @param size    条数
     * @param keyword 用户组名称 / 标识检索关键字
     * @return 用户组列表
     */
    @Override
    public PageData<UserGroupResponseDto> list(int page, int size, String keyword) {
        // 1. 数据库操作
        Page<UserGroup> pageRequest = new Page<>(page, size);
        List<UserGroup> userGroups;
        if (StringUtils.isNotEmpty(keyword)) {
            userGroups = super.list(pageRequest, Wrappers.<UserGroup>lambdaQuery().like(UserGroup::getUserGroupName, keyword).or(o -> o.like(UserGroup::getUserGroupCode, keyword)).orderByAsc(UserGroup::getUserGroupCode));
        } else {
            userGroups = super.list(pageRequest, Wrappers.<UserGroup>lambdaQuery().orderByAsc(UserGroup::getUserGroupCode));
        }

        // 2. 属性设置
        PageData<UserGroupResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var records = CommonUtil.stream(userGroups).map(userGroup -> {
            UserGroupResponseDto userGroupResponse = new UserGroupResponseDto();
            userGroupResponse.setId(userGroup.getUserGroupId());
            userGroupResponse.setName(userGroup.getUserGroupName());
            userGroupResponse.setCode(userGroup.getUserGroupCode());
            userGroupResponse.setType(UserGroupType.valueOf(userGroup.getUserGroupType()));

            // 2.1 查询成员数
            if (UserGroupType.STATIC.name().equals(userGroup.getUserGroupType())) {
                long cnt = userGroupMappingService.count(Wrappers.<UserGroupMapping>lambdaQuery().eq(UserGroupMapping::getUserGroupId, userGroup.getUserGroupId()));
                userGroupResponse.setMemberNum(cnt);
            }
            return userGroupResponse;
        }).toList();
        pageData.setList(records);
        return pageData;
    }

    /**
     * 移除用户组映射
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.USER_GROUP,
            sysOperation = SysOperationType.DELETE,
            success = "删除了用户组（{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}" +
                    "）中的用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）",
            fail = "删除用户组（{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}" +
                    "）中的用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）失败"
    )
    @Transactional
    @Override
    public void removeUserGroupMapping(UserGroupMappingRequestDto requestDto) {

        // 1. 待删除的映射关系
        List<UserGroupMapping> deleteTargetMappings = getMappings(requestDto);
        if (CollectionUtils.isNotEmpty(deleteTargetMappings)) {

            // 2. 数据库操作
            SqlHelper.executeBatch(getSqlSessionFactory(), this.log, deleteTargetMappings, deleteTargetMappings.size(), ((sqlSession, userUserGroupMapping) -> {
                UserGroupMappingMapper mapper = sqlSession.getMapper(UserGroupMappingMapper.class);
                mapper.delete(Wrappers.<UserGroupMapping>lambdaQuery().eq(UserGroupMapping::getUserId, userUserGroupMapping.getUserId()).and(o -> o.eq(UserGroupMapping::getUserGroupId, userUserGroupMapping.getUserGroupId())));
            }));
        }
    }

    /**
     * 删除指定用户的全部用户组映射
     *
     * @param userId 用户UID
     */
    @Transactional
    @Override
    public void removeUserGroupMapping(String userId) {
        userGroupMappingService.remove(Wrappers.<UserGroupMapping>lambdaQuery().eq(UserGroupMapping::getUserId, userId));
    }

    /**
     * 获取用户组详情
     *
     * @param userGroupId 用户组 ID
     * @return 用户组详情
     */
    @Override
    public UserGroupResponseDto detail(String userGroupId) {
        UserGroupResponseDto userGroupResponse = new UserGroupResponseDto();
        // 1. 获取基本信息
        UserGroup userGroup = super.getById(userGroupId);
        if (userGroup == null) {
            return userGroupResponse;
        }

        userGroupResponse.setId(userGroup.getUserGroupId());
        userGroupResponse.setName(userGroup.getUserGroupName());
        userGroupResponse.setCode(userGroup.getUserGroupCode());
        userGroupResponse.setType(UserGroupType.valueOf(userGroup.getUserGroupType()));
        userGroupResponse.setDesc(userGroup.getDescription());

        if (UserGroupType.DYNAMIC.name().equals(userGroup.getUserGroupType())) {
            userGroupResponse.setConditions(CommonUtil.nonJdkDeserializeObject(userGroup.getDynamicConditions(), DynamicUserGroupConditionsDto.class));
        }
        return userGroupResponse;
    }

    /**
     * 获取组内用户
     *
     * @param page        页数
     * @param size        条数
     * @param userGroupId 用户组ID
     * @param keyword     用户名 / 邮箱 / 手机号检索关键字
     * @return 组内用户集合
     */
    @Override
    public PageData<UserResponseDto> getGroupUsers(int page, int size, String userGroupId, String keyword) {
        PageData<UserResponseDto> pageData = new PageData<>();

        // 1. 查询用户组
        UserGroup userGroup = super.getById(userGroupId);
        if (userGroup == null) {
            return pageData;
        }

        // 2. 查询数据库
        List<User> users = new ArrayList<>();
        // 2.1 静态数组组
        if (UserGroupType.STATIC.name().equals(userGroup.getUserGroupType())) {
            Page<User> pageRequest = new Page<>(page, size);
            userGroupRepository.searchGroupUsers(pageRequest, userGroupId, keyword);

            pageData.setCurrent(pageRequest.getCurrent());
            pageData.setTotal(pageRequest.getTotal());
            pageData.setPages(pageRequest.getPages());
            pageData.setSize(pageRequest.getSize());

            users.addAll(pageRequest.getRecords());
        }

        // 2.2 动态用户组
        if (UserGroupType.DYNAMIC.name().equals(userGroup.getUserGroupType())) {
            var conditions = CommonUtil.nonJdkDeserializeObject(userGroup.getDynamicConditions(), DynamicUserGroupConditionsDto.class);
            int offset = (page - 1) * size;
            QueryWrapper<User> queryWrapper = new QueryWrapper<>();
            queryWrapper.eq(CommonUtil.extractFieldNameFromGetter(User::getDeleted), false);
            if (StringUtils.isNotEmpty(keyword)) {
                queryWrapper.and(q ->
                        q.like("t_user.username", keyword)
                                .or().like("t_user.email_address", keyword)
                                .or().like("t_user.phone_number", keyword)
                );
            }
            queryWrapper.and(q -> editQueryWrapper(q, conditions));

            long total = userRepository.countUsers(queryWrapper);
            long pages;
            if (size > 0) {
                pages = (long) Math.ceil((double) total / size);
            } else {
                pages = 1;
            }

            if (page > pages) {
                page = 1;
            }

            pageData.setTotal(total);
            pageData.setPages(pages);
            pageData.setSize((long) size);
            pageData.setCurrent((long) page);

            users.addAll(userRepository.searchUsers(queryWrapper, size, offset));
        }

        pageData.setList(CommonUtil.stream(users).map(user -> {
            UserResponseDto userResponse = new UserResponseDto();
            userResponse.setId(user.getUserId());
            userResponse.setUsername(user.getUsername());
            userResponse.setEmailAddress(user.getEmailAddress());
            userResponse.setPhoneNumber(user.getPhoneNumber());
            return userResponse;
        }).toList());
        return pageData;
    }

    /**
     * 更新用户组
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.USER_GROUP,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了用户组（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).USER_GROUP) }}）",
            fail = "修改用户组（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).USER_GROUP) }}）失败"
    )
    @Transactional
    @Override
    public void updateUserGroup(UserGroupRequestDto requestDto) {
        String userGroupId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();
        compareObjBuilder.id(userGroupId);

        // 1. 获取版本号
        var rawUserGroup = super.getById(userGroupId);
        if (Objects.isNull(rawUserGroup)) {
            return;
        }
        compareObjBuilder.before(rawUserGroup);

        // 2. 检查用户标识是否存在
        checkUserGroupCode(requestDto, rawUserGroup);

        // 3. 动态用户组参数校验
        if (UserGroupType.DYNAMIC.name().equals(rawUserGroup.getUserGroupType())) {
            checkDynamicUserGroupConditions(requestDto);
            CommonUtil.validateBean(requestDto, UserGroupRequestDto.DynamicUserGroup.class);
        }

        // 4. 属性编辑
        UserGroup updateUserGroup = new UserGroup();
        updateUserGroup.setUserGroupId(userGroupId);
        updateUserGroup.setUserGroupName(requestDto.getName());
        updateUserGroup.setUserGroupCode(requestDto.getCode());
        updateUserGroup.setDescription(requestDto.getDesc());
        updateUserGroup.setVersion(rawUserGroup.getVersion());
        if (UserGroupType.DYNAMIC.name().equals(rawUserGroup.getUserGroupType())) {
            updateUserGroup.setDynamicConditions(CommonUtil.nonJdkSerializeObject(requestDto.getConditions()));
        }

        // 5. 数据库操作
        super.updateById(updateUserGroup);

        compareObjBuilder.after(super.getById(userGroupId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 删除用户组
     *
     * @param userGroupId 用户组 ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.USER_GROUP,
            sysOperation = SysOperationType.DELETE,
            success = "删除了用户组（{{ @linkGen.toLink(#userGroupId, T(ResourceType).USER_GROUP) }}）",
            fail = "删除用户组（{{ @linkGen.toLink(#userGroupId, T(ResourceType).USER_GROUP) }}）失败"
    )
    @Transactional
    @Override
    public void removeUserGroup(String userGroupId) {
        // 1. 删除用户组
        super.removeById(userGroupId);

        // 2. 删除关联的全部用户组映射关系
        userGroupMappingService.remove(Wrappers.<UserGroupMapping>lambdaQuery().eq(UserGroupMapping::getUserGroupId, userGroupId));

        // 3. 删除关联的全部角色映射关系
        roleService.removeUserRoleMapping(userGroupId);

        // 4. 删除关联的授权
        authorizeService.removeAuthorization(userGroupId);
    }

    /**
     * 获取权限
     *
     * @param page                           页数
     * @param size                           条数
     * @param userGroupId                    用户组ID
     * @param resourceGroupNameSearchKeyword 资源组名称搜索关键字
     * @param resourceNameSearchKeyword      资源名称搜索关键字
     * @param permissionNameSearchKeyword    权限名称搜索关键字
     * @param permissionCodeSearchKeyword    权限标识搜索关键字
     * @return 权限信息
     */
    @Override
    public PageData<PermissionResponseDto> getPermissions(int page, int size, String userGroupId, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword) {
        // 1. 查询数据库
        Page<AuthorizeRecord> pageRequest = new Page<>(page, size);
        permissionService.getUserGroupPermissions(pageRequest, userGroupId, resourceGroupNameSearchKeyword, resourceNameSearchKeyword, permissionNameSearchKeyword, permissionCodeSearchKeyword);

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

    private List<UserGroupMapping> getMappings(UserGroupMappingRequestDto requestDto) {
        var userIds = CommonUtil.stream(requestDto.getUserIds()).filter(StringUtils::isNotBlank).collect(Collectors.toSet());
        var userGroupIds = CommonUtil.stream(requestDto.getUserGroupIds()).filter(StringUtils::isNotBlank).collect(Collectors.toSet());

        List<UserGroupMapping> mappings = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(userIds) && CollectionUtils.isNotEmpty(userGroupIds)) {
            userIds.forEach(userId -> userGroupIds.forEach(userGroupId -> {
                UserGroupMapping mapping = new UserGroupMapping();
                mapping.setUserId(userId);
                mapping.setUserGroupId(userGroupId);
                mappings.add(mapping);
            }));
        }
        return mappings;
    }

    private void checkUserGroupCode(UserGroupRequestDto requestDto, UserGroup rawUserGroup) {
        if (Objects.nonNull(rawUserGroup) && StringUtils.equals(requestDto.getCode(), rawUserGroup.getUserGroupCode())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<UserGroup>lambdaQuery().eq(UserGroup::getUserGroupCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.USER_GROUP_MSG_1000, requestDto.getCode());
        }
    }

    private void checkDynamicUserGroupConditions(UserGroupRequestDto requestDto) {
        if (CollectionUtils.isEmpty(requestDto.getConditions().getFilters()) && CollectionUtils.isEmpty(requestDto.getConditions().getGroups())) {
            throw new BizException(MessageConstants.USER_GROUP_MSG_1001);
        }
    }

    private void editQueryWrapper(QueryWrapper<User> queryWrapper, DynamicUserGroupConditionsDto conditions) {
        if (CollectionUtils.isNotEmpty(conditions.getFilters())) {
            for (DataFilterDto filter : conditions.getFilters()) {
                AuthUtil.editQuery(queryWrapper, filter, conditions.getConjunction());
            }
        }

        if (CollectionUtils.isNotEmpty(conditions.getGroups())) {
            if (ConjunctionType.OR.equals(conditions.getConjunction())) {
                for (DynamicUserGroupConditionsDto group : conditions.getGroups()) {
                    queryWrapper.or(q -> editQueryWrapper(q, group));
                }
            } else {
                for (DynamicUserGroupConditionsDto group : conditions.getGroups()) {
                    queryWrapper.and(q -> editQueryWrapper(q, group));
                }
            }
        }
    }
}
