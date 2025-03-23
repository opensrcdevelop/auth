package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.*;
import cn.opensrcdevelop.auth.biz.entity.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.entity.UserGroup;
import cn.opensrcdevelop.auth.biz.entity.UserGroupMapping;
import cn.opensrcdevelop.auth.biz.mapper.UserGroupMapper;
import cn.opensrcdevelop.auth.biz.mapper.UserGroupMappingMapper;
import cn.opensrcdevelop.auth.biz.repository.UserGroupRepository;
import cn.opensrcdevelop.auth.biz.service.*;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
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
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class UserGroupServiceImpl extends ServiceImpl<UserGroupMapper, UserGroup> implements UserGroupService {

    private final UserGroupMappingService userGroupMappingService;
    private final UserGroupRepository userGroupRepository;
    private final RoleService roleService;
    private final AuthorizeService authorizeService;
    private final PermissionService permissionService;

    /**
     * 创建用户组
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createUserGroup(UserGroupRequestDto requestDto) {
        // 1. 检查用户标识是否存在
        checkUserGroupCode(requestDto, null);

        // 2. 属性设置
        UserGroup userGroup = new UserGroup();
        userGroup.setUserGroupId(CommonUtil.getUUIDString());
        userGroup.setUserGroupName(requestDto.getName());
        userGroup.setUserGroupCode(requestDto.getCode());
        userGroup.setDescription(requestDto.getDesc());

        // 3. 数据库操作
        super.save(userGroup);
    }

    /**
     * 创建用户组映射
     *
     * @param requestDto 请求
     */
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
     * 获取用户组信息
     *
     * @param userId 用户 ID
     * @return 用户组信息
     */
    @Override
    public List<UserGroup> getUserGroups(String userId) {
        return userGroupRepository.searchUserGroups(userId);
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

            // 2.1 查询成员数
            long cnt = userGroupMappingService.count(Wrappers.<UserGroupMapping>lambdaQuery().eq(UserGroupMapping::getUserGroupId, userGroup.getUserGroupId()));
            userGroupResponse.setMemberNum(cnt);
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
        userGroupResponse.setDesc(userGroup.getDescription());
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
        // 1. 查询数据库
        Page<User> pageRequest = new Page<>(page, size);
        userGroupRepository.searchGroupUsers(pageRequest, userGroupId, keyword);

        // 2. 属性编辑
        PageData<UserResponseDto> pageData = new PageData<>();
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setSize(pageRequest.getSize());

        var users = CommonUtil.stream(pageRequest.getRecords()).map(user -> {
            UserResponseDto userResponse = new UserResponseDto();
            userResponse.setId(user.getUserId());
            userResponse.setUsername(user.getUsername());
            userResponse.setEmailAddress(user.getEmailAddress());
            userResponse.setPhoneNumber(user.getPhoneNumber());
            return userResponse;
        }).toList();
        pageData.setList(users);
        return pageData;
    }

    /**
     * 更新用户组
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void updateUserGroup(UserGroupRequestDto requestDto) {
        // 1. 获取版本号
        var rawUserGroup = super.getById(requestDto.getId());
        if (Objects.isNull(rawUserGroup)) {
            return;
        }

        // 2. 检查用户标识是否存在
        checkUserGroupCode(requestDto, rawUserGroup);

        // 3. 属性编辑
        UserGroup updateUserGroup = new UserGroup();
        updateUserGroup.setUserGroupId(requestDto.getId());
        updateUserGroup.setUserGroupName(requestDto.getName());
        updateUserGroup.setUserGroupCode(requestDto.getCode());
        updateUserGroup.setDescription(requestDto.getDesc());
        updateUserGroup.setVersion(rawUserGroup.getVersion());

        // 4. 数据库操作
        super.updateById(updateUserGroup);
    }

    /**
     * 删除用户组
     *
     * @param userGroupId 用户组 ID
     */
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
                condition.setExpression(exp.getExpression());
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
}
