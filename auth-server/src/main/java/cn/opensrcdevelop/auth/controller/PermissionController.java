package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.annocation.ResourceLimit;
import cn.opensrcdevelop.auth.biz.dto.*;
import cn.opensrcdevelop.auth.biz.service.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.PermissionExpService;
import cn.opensrcdevelop.auth.biz.service.PermissionService;
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

import java.util.List;

@Tag(name = "API-Permission", description = "接口-权限管理")
@RestController
@RestResponse
@RequestMapping("/permission")
@RequiredArgsConstructor
public class PermissionController {

    private final PermissionService permissionService;
    private final AuthorizeService authorizeService;
    private final PermissionExpService permissionExpService;

    @Operation(summary = "创建权限", description = "创建权限")
    @PostMapping
    @Authorize({ "allPermPermissions", "createPermission" })
    @ResourceLimit(ids = {
            "14ee7b7e-db4c-40cc-b93e-d38969be5542",
            "75a3dee9-a95f-4ad3-a32a-a7f6d34c0050",
            "931848a2-beb6-444e-a7d7-72e909553b00",
            "0f35efeb-3f5a-4e22-84c4-4b4a08b6717c",
            "75c35dc3-1996-48ab-be27-e4078f86a559",
            "79a30d3a-0fde-4087-a307-619cc0c56b17",
            "edd2a541-f482-45cd-9842-c1ebf43c346c",
            "911e08a0-d91a-4c66-8a7d-c8fda2c79c69",
            "4d367bc0-d043-402c-a1d5-d4e5c55c9e23",
            "df35c2ee-f8fb-4a3e-8627-879d2bcd23cc",
            "97392350-5214-4dbb-83e8-45b678ce145e",
            "da2c6573-d236-4e4d-96a4-85c517b72c59",
            "1624ca73-e656-48d9-800e-b5762b51d7c5",
            "6df389ec-09be-4443-a80a-b3fed5d9b9d8"

    }, idEl = "#requestDto.resourceId")
    public void createPermission(@RequestBody @Validated(ValidationGroups.Operation.INSERT.class) PermissionRequestDto requestDto) {
        permissionService.createPermission(requestDto);
    }

    @Operation(summary = "授权", description = "授权")
    @PostMapping("/authorize")
    @Authorize({ "allPermPermissions", "authorizePermission" })
    @ResourceLimit(ids = {
            "14ee7b7e-db4c-40cc-b93e-d38969be5542",
            "75a3dee9-a95f-4ad3-a32a-a7f6d34c0050",
            "931848a2-beb6-444e-a7d7-72e909553b00",
            "0f35efeb-3f5a-4e22-84c4-4b4a08b6717c",
            "75c35dc3-1996-48ab-be27-e4078f86a559",
            "79a30d3a-0fde-4087-a307-619cc0c56b17",
            "edd2a541-f482-45cd-9842-c1ebf43c346c",
            "911e08a0-d91a-4c66-8a7d-c8fda2c79c69",
            "4d367bc0-d043-402c-a1d5-d4e5c55c9e23",
            "df35c2ee-f8fb-4a3e-8627-879d2bcd23cc",
            "97392350-5214-4dbb-83e8-45b678ce145e",
            "da2c6573-d236-4e4d-96a4-85c517b72c59",
            "1624ca73-e656-48d9-800e-b5762b51d7c5",
            "6df389ec-09be-4443-a80a-b3fed5d9b9d8"

    }, idEl = "#requestDto.resourceId")
    public void authorize(@RequestBody @Valid AuthorizeRequestDto requestDto) {
        authorizeService.authorize(requestDto);
    }

    @Operation(summary = "创建权限表达式", description = "创建权限表达式")
    @PostMapping("/exp")
    @Authorize({ "allPermissionExpPermissions", "createPermissionExp" })
    public void createPermissionExpression(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) PermissionExpRequestDto requestDto) {
        permissionExpService.createPermissionExp(requestDto);
    }

    @Operation(summary = "获取当前用户权限", description = "获取当前用户权限")
    @GetMapping("/me")
    public List<PermissionResponseDto> getCurrentUserPermissions() {
        return permissionService.getCurrentUserPermissions();
    }

    @Operation(summary = "取消授权", description = "取消授权")
    @Parameters({
            @Parameter(name = "permissionId", description = "权限ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "principalId", description = "用户 / 用户组 / 角色ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/authorize/{permissionId}/{principalId}")
    @Authorize({ "allPermPermissions", "cancelAuthorization" })
    @ResourceLimit(ids = {
            "baec302c-39ac-4e51-9d28-fb8c9c43caa3",
            "3c6cdb57-7a78-4680-b1f2-3da0673bd883",
            "4a7eb192-b0e8-4678-bf81-bbbd70ba1880"
    },idEl = "#principalId")
    public void cancelAuthorization(@PathVariable String permissionId, @PathVariable String principalId) {
        authorizeService.removeAuthorization(permissionId, principalId);
    }

    @Operation(summary = "获取权限详情", description = "获取权限详情")
    @Parameters({
            @Parameter(name = "id", description = "权限ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "keyword", description = "被授权主体关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/{id}")
    @Authorize({ "allPermPermissions", "getPermissionDetail" })
    public PermissionResponseDto detail(@PathVariable @NotBlank String id, @RequestParam(required = false) String keyword) {
        return permissionService.detail(id, keyword);
    }

    @Operation(summary = "删除权限", description = "删除权限")
    @Parameters({
            @Parameter(name = "id", description = "权限ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/{id}")
    @Authorize({ "allPermPermissions", "deletePermission" })
    public void removePermission(@PathVariable @NotBlank String id) {
        permissionService.removePermission(id);
    }

    @Operation(summary = "更新权限", description = "更新权限")
    @PutMapping
    @Authorize({ "allPermPermissions", "updatePermission" })
    @ResourceLimit(ids = {
            "14ee7b7e-db4c-40cc-b93e-d38969be5542",
            "75a3dee9-a95f-4ad3-a32a-a7f6d34c0050",
            "931848a2-beb6-444e-a7d7-72e909553b00",
            "0f35efeb-3f5a-4e22-84c4-4b4a08b6717c",
            "75c35dc3-1996-48ab-be27-e4078f86a559",
            "79a30d3a-0fde-4087-a307-619cc0c56b17",
            "edd2a541-f482-45cd-9842-c1ebf43c346c",
            "911e08a0-d91a-4c66-8a7d-c8fda2c79c69",
            "4d367bc0-d043-402c-a1d5-d4e5c55c9e23",
            "df35c2ee-f8fb-4a3e-8627-879d2bcd23cc",
            "97392350-5214-4dbb-83e8-45b678ce145e",
            "da2c6573-d236-4e4d-96a4-85c517b72c59",
            "1624ca73-e656-48d9-800e-b5762b51d7c5",
            "6df389ec-09be-4443-a80a-b3fed5d9b9d8"

    }, idEl = "#requestDto.resourceId")
    public void updatePermission(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) PermissionRequestDto requestDto) {
        permissionService.updatePermission(requestDto);
    }

    @Operation(summary = "获取权限表达式列表", description = "获取权限表达式列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "表达式 / 名称检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/exp/list")
    @Authorize({ "allPermPermissions", "listPermissionExp" })
    public PageData<PermissionExpResponseDto> listPermissionExp(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return permissionExpService.list(page, size, keyword);
    }

    @Operation(summary = "获取权限表达式详情", description = "获取权限表达式详情")
    @Parameters({
            @Parameter(name = "id", description = "权限表达式ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/exp/{id}")
    @Authorize({ "allPermissionExpPermissions", "getPermissionExpDetail" })
    public PermissionExpResponseDto expDetail(@PathVariable @NotBlank String id) {
        return permissionExpService.detail(id);
    }

    @Operation(summary = "添加授权条件", description = "添加授权条件")
    @PostMapping("/authorize/cond")
    @Authorize({ "allAuthorizeCondPermissions", "addAuthorizeCondition" })
    public void createAuthorizeCondition(@RequestBody AuthorizeConditionRequestDto requestDto) {
        authorizeService.createAuthorizeCondition(requestDto);
    }

    @Operation(summary = "删除授权条件", description = "删除授权条件")
    @DeleteMapping("/authorize/cond")
    @Authorize({ "allAuthorizeCondPermissions", "deleteAuthorizeCondition" })
    public void removeAuthorizeCondition(@RequestBody AuthorizeConditionRequestDto requestDto) {
        authorizeService.removeAuthorizeCondition(requestDto);
    }

    @Operation(summary = "更新权限表达式", description = "更新权限表达式")
    @PutMapping("/exp")
    @Authorize({ "allPermissionExpPermissions", "updatePermissionExp" })
    public void updatePermissionExpression(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) PermissionExpRequestDto requestDto) {
        permissionExpService.updatePermissionExp(requestDto);
    }

    @Operation(summary = "删除权限表达式", description = "删除权限表达式")
    @Parameters({
            @Parameter(name = "id", description = "权限表达式ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/exp/{id}")
    @Authorize({ "allPermissionExpPermissions", "deletePermissionExp" })
    public  void removePermissionExpression(@PathVariable @NotBlank String id) {
        permissionExpService.removePermissionExp(id);
    }
}
