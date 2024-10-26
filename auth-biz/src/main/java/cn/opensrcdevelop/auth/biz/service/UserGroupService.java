package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.*;
import cn.opensrcdevelop.auth.biz.entity.UserGroup;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface UserGroupService extends IService<UserGroup> {

    void createUserGroup(UserGroupRequestDto requestDto);

    void createUserGroupMapping(UserGroupMappingRequestDto requestDto);

    List<UserGroup> getUserGroups(String userId);

    PageData<UserGroupResponseDto> list(int page, int size, String keyword);

    void removeUserGroupMapping(UserGroupMappingRequestDto requestDto);

    void removeUserGroupMapping(String userId);

    UserGroupResponseDto detail(String userGroupId);

    PageData<UserResponseDto> getGroupUsers(int page, int size, String userGroupId, String keyword);

    void updateUserGroup(UserGroupRequestDto requestDto);

    void removeUserGroup(String userGroupId);

    PageData<PermissionResponseDto> getPermissions(int page, int size, String userGroupId, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);
}
