package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.annocation.ResourceLimit;
import cn.opensrcdevelop.auth.biz.dto.RoleMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.RoleRequestDto;
import cn.opensrcdevelop.auth.biz.dto.RoleResponseDto;
import cn.opensrcdevelop.auth.biz.service.RoleService;
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

@Tag(name = "API-Role", description = "接口-角色管理")
@RestController
@RestResponse
@RequestMapping("/role")
@RequiredArgsConstructor
public class RoleController {

    private final RoleService roleService;

    @Operation(summary = "创建角色", description = "创建角色")
    @PostMapping
    @Authorize({ "allRolePermissions", "createRole" })
    public void createRole(@RequestBody @Validated({ValidationGroups.Operation.INSERT.class}) RoleRequestDto requestDto) {
        roleService.createRole(requestDto);
    }

    @Operation(summary = "创建用户角色映射", description = "创建用户角色映射")
    @PostMapping("/mapping")
    @Authorize({ "allRoleMappingPermissions", "createRoleMapping" })
    @ResourceLimit(ids = { "baec302c-39ac-4e51-9d28-fb8c9c43caa3" }, idEl = "#requestDto.roleIds", isList = true)
    @ResourceLimit(ids = { "4a7eb192-b0e8-4678-bf81-bbbd70ba1880" }, idEl = "#requestDto.userIds", isList = true)
    @ResourceLimit(ids = { "3c6cdb57-7a78-4680-b1f2-3da0673bd883" }, idEl = "#requestDto.userGroupIds", isList = true)
    public void createUserRoleMapping(@RequestBody @Valid RoleMappingRequestDto requestDto) {
        roleService.createUserRoleMapping(requestDto);
    }

    @Operation(summary = "删除用户角色映射", description = "删除用户角色映射")
    @DeleteMapping("/mapping")
    @Authorize({ "allRoleMappingPermissions", "deleteRoleMapping" })
    @ResourceLimit(ids = { "baec302c-39ac-4e51-9d28-fb8c9c43caa3" }, idEl = "#requestDto.roleIds", isList = true)
    @ResourceLimit(ids = { "4a7eb192-b0e8-4678-bf81-bbbd70ba1880" }, idEl = "#requestDto.userIds", isList = true)
    @ResourceLimit(ids = { "3c6cdb57-7a78-4680-b1f2-3da0673bd883" }, idEl = "#requestDto.userGroupIds", isList = true)
    public void removeUserRoleMapping(@RequestBody @Valid RoleMappingRequestDto requestDto) {
        roleService.removeUserRoleMapping(requestDto);
    }

    @Operation(summary = "获取角色列表", description = "获取角色列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "角色名称或标识检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/list")
    @Authorize({ "allRolePermissions", "listRole" })
    public PageData<RoleResponseDto> listRoles(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return roleService.listRoles(page, size, keyword);
    }

    @Operation(summary = "获取角色主体", description = "获取角色主体")
    @Parameters({
            @Parameter(name = "id", description = "角色ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "用户名 / 用户组名检索关键字", in = ParameterIn.QUERY),
    })
    @GetMapping("/{id}/principals")
    @Authorize({ "allRolePermissions", "getRolePrincipals" })
    public PageData<RoleResponseDto> getRolePrincipals(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @PathVariable @NotBlank String id, @RequestParam(required = false) String keyword) {
        return roleService.getRolePrincipals(page, size, id, keyword);
    }

    @Operation(summary = "获取角色详情", description = "获取角色详情")
    @Parameters({
            @Parameter(name = "id", description = "角色ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/{id}")
    @Authorize({ "allRolePermissions", "getRoleDetail" })
    public RoleResponseDto detail(@PathVariable @NotBlank String id) {
        return roleService.detail(id);
    }

    @Operation(summary = "更新角色", description = "更新角色")
    @PutMapping
    @Authorize({ "allRolePermissions", "updateRole" })
    @ResourceLimit(ids = { "baec302c-39ac-4e51-9d28-fb8c9c43caa3" }, idEl = "#requestDto.id")
    public void updateRole(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) RoleRequestDto requestDto) {
        roleService.updateRole(requestDto);
    }

    @Operation(summary = "删除角色", description = "删除角色")
    @Parameters({
            @Parameter(name = "id", description = "角色ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/{id}")
    @Authorize({ "allRolePermissions", "deleteRole" })
    @ResourceLimit(ids = { "baec302c-39ac-4e51-9d28-fb8c9c43caa3" }, idEl = "#id")
    public void removeRole(@PathVariable @NotBlank String id) {
        roleService.removeRole(id);
    }
}
