package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.*;
import cn.opensrcdevelop.auth.biz.service.UserGroupService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "API-UserGroup", description = "接口-用户组管理")
@RestController
@RestResponse
@RequestMapping("/userGroup")
@RequiredArgsConstructor
public class UserGroupController {

    private final UserGroupService userGroupService;

    @Operation(summary = "创建用户组", description = "创建用户组")
    @PostMapping
    @Authorize({ "allUserGroupPermissions", "createUserGroup" })
    public void createUserGroup(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) UserGroupRequestDto requestDto) {
        userGroupService.createUserGroup(requestDto);
    }

    @Operation(summary = "创建用户组映射", description = "创建用户组映射")
    @PostMapping("/mapping")
    @Authorize({ "allUserGroupMappingPermissions", "createUserGroupMapping" })
    public void createUserUserGroupMapping(@RequestBody @Valid UserGroupMappingRequestDto requestDto) {
        userGroupService.createUserGroupMapping(requestDto);
    }

    @Operation(summary = "获取用户组列表", description = "获取用户组列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "用户组名称或标识关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/list")
    @Authorize({ "allUserGroupPermissions", "listUserGroup" })
    public PageData<UserGroupResponseDto> list(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return userGroupService.list(page, size, keyword);
    }

    @Operation(summary = "删除用户组映射", description = "删除用户组映射")
    @DeleteMapping("/mapping")
    @Authorize({ "allUserGroupMappingPermissions", "deleteUserGroupMapping" })
    public void removeUserUserGroupMapping(@RequestBody @Valid UserGroupMappingRequestDto requestDto) {
        userGroupService.removeUserGroupMapping(requestDto);
    }

    @Operation(summary = "获取用户组详情", description = "获取用户组详情")
    @Parameters({
            @Parameter(name = "id", description = "用户组ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/{id}")
    @Authorize({ "allUserGroupPermissions", "getUserGroupDetail" })
    public UserGroupResponseDto detail(@PathVariable @NotBlank String id) {
        return userGroupService.detail(id);
    }

    @Operation(summary = "获取组内用户", description = "获取组内用户")
    @Parameters({
            @Parameter(name = "id", description = "用户组ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "用户名 / 邮箱 / 手机号检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/{id}/users")
    @Authorize({ "allUserGroupPermissions", "getUserGroupUsers" })
    public PageData<UserResponseDto> getGroupUsers(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @PathVariable @NotBlank String id, @RequestParam(required = false) String keyword) {
        return userGroupService.getGroupUsers(page, size, id, keyword);
    }

    @Operation(summary = "更新用户组", description = "更新用户组")
    @PutMapping
    @Authorize({ "allUserGroupPermissions", "updateUserGroup" })
    public void updateUserGroup(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) UserGroupRequestDto requestDto ) {
        userGroupService.updateUserGroup(requestDto);
    }

    @Operation(summary = "删除用户组", description = "删除用户组")
    @Parameters({
            @Parameter(name = "id", description = "用户组ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/{id}")
    @Authorize({ "allUserGroupPermissions", "deleteUserGroup" })
    public void removeUserGroup(@PathVariable String id) {
        userGroupService.removeUserGroup(id);
    }

    @Operation(summary = "获取用户组权限", description = "获取用户组权限")
    @Parameters({
            @Parameter(name = "id", description = "用户组ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "resourceGroupNameSearchKeyword", description = "资源组名称检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "resourceNameSearchKeyword", description = "资源名称检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "permissionNameSearchKeyword", description = "权限名称检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "permissionCodeSearchKeyword", description = "权限标识检索关键字", in = ParameterIn.QUERY),
    })
    @GetMapping("/{id}/permissions")
    @Authorize({ "allUserGroupPermissions", "getUserGroupPermissions" })
    public PageData<PermissionResponseDto> getPermissions(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @PathVariable @NotBlank String id,
                                                          @RequestParam(required = false) String resourceGroupNameSearchKeyword,
                                                          @RequestParam(required = false) String resourceNameSearchKeyword,
                                                          @RequestParam(required = false) String permissionNameSearchKeyword,
                                                          @RequestParam(required = false) String permissionCodeSearchKeyword) {
        return userGroupService.getPermissions(page, size, id, resourceGroupNameSearchKeyword, resourceNameSearchKeyword, permissionNameSearchKeyword, permissionCodeSearchKeyword);
    }
}
