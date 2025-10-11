package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeConditionRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.VerifyPermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.VerifyPermissionsRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.DebugPermissionExpRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.DebugPermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamConfigDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateResponseDto;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.permission.PermissionService;
import cn.opensrcdevelop.auth.biz.service.permission.expression.PermissionExpService;
import cn.opensrcdevelop.auth.biz.service.permission.expression.PermissionExpTemplateService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.EnumValue;
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
    private final PermissionExpTemplateService permissionExpTemplateService;

    @Operation(summary = "创建权限", description = "创建权限")
    @PostMapping
    @Authorize({ "allPermPermissions", "createPermission" })
    public void createPermission(@RequestBody @Validated(ValidationGroups.Operation.INSERT.class) PermissionRequestDto requestDto) {
        permissionService.createPermission(requestDto);
    }

    @Operation(summary = "授权", description = "授权")
    @PostMapping("/authorize")
    @Authorize({ "allPermPermissions", "authorizePermission" })
    public void authorize(@RequestBody @Valid AuthorizeRequestDto requestDto) {
        authorizeService.authorize(requestDto);
    }

    @Operation(summary = "创建权限表达式", description = "创建权限表达式")
    @PostMapping("/exp")
    @Authorize({ "allPermissionExpPermissions", "createPermissionExp" })
    public void createPermissionExpression(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) PermissionExpRequestDto requestDto) {
        permissionExpService.createPermissionExp(requestDto);
    }

    @Operation(summary = "校验权限", description = "校验权限")
    @PostMapping("/verify")
    public List<VerifyPermissionResponseDto> verifyPermissions(@RequestBody @Valid VerifyPermissionsRequestDto requestDto) {
        return permissionService.verifyPermissions(requestDto);
    }

    @Operation(summary = "取消授权", description = "取消授权")
    @Parameters({
            @Parameter(name = "permissionId", description = "权限ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "principalId", description = "用户 / 用户组 / 角色ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/authorize/{permissionId}/{principalId}")
    @Authorize({ "allPermPermissions", "cancelAuthorization" })
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

    @Operation(summary = "获取权限表达式关联的权限列表", description = "获取权限表达式关联的权限列表")
    @Parameters({
            @Parameter(name = "id", description = "权限表达式ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/exp/{id}/permissions")
    public List<PermissionResponseDto> expPermissions(@PathVariable @NotBlank String id) {
        return permissionExpService.expPermissions(id);
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

    @Operation(summary = "调试权限表达式", description = "调试权限表达式")
    @PostMapping("/exp/debug")
    @Authorize({ "allPermissionExpPermissions", "debugPermissionExp" })
    public DebugPermissionExpResponseDto debugPermissionExpression(@RequestBody DebugPermissionExpRequestDto requestDto) {
        return permissionExpService.debugPermissionExp(requestDto);
    }

    @Operation(summary = "更新授权优先级", description = "更新授权优先级")
    @Parameters({
            @Parameter(name = "id", description = "授权ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "priority", description = "优先级", in = ParameterIn.PATH, required = true),
    })
    @PutMapping("/authorize/{id}/{priority}")
    @Authorize({ "allPermPermissions", "updateAuthorizePriority" })
    public void updateAuthorizePriority(@PathVariable @NotBlank String id, @PathVariable @EnumValue({ "-1", "0", "1", "2", "3" }) Integer priority) {
        authorizeService.updateAuthorizePriority(id, priority);
    }

    @Operation(summary = "创建权限表达式模板", description = "创建权限表达式模板")
    @PostMapping("/exp/template")
    @Authorize({ "allPermissionExpTemplatePermissions", "createPermissionExpTemplate" })
    public void createPermissionExpTemplate(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) PermissionExpTemplateRequestDto requestDto) {
        permissionExpTemplateService.createPermissionExpTemplate(requestDto);
    }

    @Operation(summary = "更新权限表达式模板", description = "更新权限表达式模板")
    @PutMapping("/exp/template")
    @Authorize({ "allPermissionExpTemplatePermissions", "updatePermissionExpTemplate" })
    public void updatePermissionExpTemplate(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) PermissionExpTemplateRequestDto requestDto) {
        permissionExpTemplateService.updatePermissionExpTemplate(requestDto);
    }

    @Operation(summary = "删除权限表达式模板", description = "删除权限表达式模板")
    @Parameters({
            @Parameter(name = "id", description = "权限表达式模板ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/exp/template/{id}")
    @Authorize({ "allPermissionExpTemplatePermissions", "deletePermissionExpTemplate" })
    public void deletePermissionExpTemplate(@PathVariable @NotBlank String id) {
        permissionExpTemplateService.deletePermissionExpTemplate(id);
    }

    @Operation(summary = "获取权限表达式模板列表", description = "获取权限表达式模板列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "权限表达式模板名称检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/exp/template/list")
    @Authorize({ "allPermissionExpTemplatePermissions", "listPermissionExpTemplate" })
    public PageData<PermissionExpTemplateResponseDto> listPermissionExpTemplate(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return permissionExpTemplateService.list(page, size, keyword);
    }

    @Operation(summary = "获取权限表达式模板详情", description = "获取权限表达式模板详情")
    @Parameters({
            @Parameter(name = "id", description = "权限表达式模板ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/exp/template/{id}")
    @Authorize({ "allPermissionExpTemplatePermissions", "getPermissionExpTemplateDetail" })
    public PermissionExpTemplateResponseDto detailPermissionExpTemplate(@PathVariable @NotBlank String id) {
        return permissionExpTemplateService.detail(id);
    }

    @Operation(summary = "获取权限表达式模板参数配置", description = "获取权限表达式模板参数配置")
    @Parameters({
            @Parameter(name = "id", description = "权限表达式模板ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/exp/template/{id}/params")
    @Authorize({ "allPermissionExpTemplatePermissions", "getPermissionExpTemplateParamConfigs" })
    public List<PermissionExpTemplateParamConfigDto> permissionExpTemplateParamConfigs(@PathVariable @NotBlank String id) {
        return permissionExpTemplateService.getParamsConfigs(id);
    }

    @Operation(summary = "获取模板关联的权限表达式列表", description = "获取模板关联的权限表达式列表")
    @Parameters({
            @Parameter(name = "id", description = "权限表达式模板ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/exp/template/{id}/exps")
    @Authorize({ "allPermissionExpTemplatePermissions", "getPermissionExpTemplateExps" })
    public List<PermissionExpResponseDto> listTemplatePermissionExp(@PathVariable @NotBlank String id) {
        return permissionExpTemplateService.getPermissionExpList(id);
    }
}
