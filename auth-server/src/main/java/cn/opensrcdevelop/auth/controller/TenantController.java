package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.tenant.annoation.TenantLimit;
import cn.opensrcdevelop.tenant.dto.CheckTenantResponseDto;
import cn.opensrcdevelop.tenant.dto.TenantRequestDto;
import cn.opensrcdevelop.tenant.dto.TenantResponseDto;
import cn.opensrcdevelop.tenant.service.TenantService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "API - Tenant", description = "租户管理")
@RestController
@RestResponse
@RequestMapping("/tenant")
@RequiredArgsConstructor
public class TenantController {

    private final TenantService tenantService;

    @Operation(summary = "创建租户", description = "创建租户")
    @PostMapping
    @TenantLimit("master")
    @PreAuthorize("@pms.hasAnyPermission('allTenantPermissions', 'createTenant')")
    public void createTenant(@RequestBody @Validated(ValidationGroups.Operation.INSERT.class) TenantRequestDto requestDto) {
        tenantService.createTenant(requestDto);
    }

    @Operation(summary = "获取租户列表", description = "获取租户列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "租户名称或标识检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/list")
    @TenantLimit("master")
    @PreAuthorize("@pms.hasAnyPermission('allTenantPermissions', 'listTenant')")
    public PageData<TenantResponseDto> listTenants(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return tenantService.listTenants(page, size,keyword);
    }

    @Operation(summary = "更新租户", description = "更新租户")
    @PutMapping
    @TenantLimit("master")
    @PreAuthorize("@pms.hasAnyPermission('allTenantPermissions', 'updateTenant')")
    public void updateTenant(@RequestBody @Validated({ValidationGroups.Operation.UPDATE.class}) TenantRequestDto requestDto) {
        tenantService.updateTenant(requestDto);
    }

    @Operation(summary = "获取租户详情", description = "获取租户详情")
    @Parameters({
            @Parameter(name = "id", description = "租户 ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/{id}")
    @TenantLimit("master")
    @PreAuthorize("@pms.hasAnyPermission('allTenantPermissions', 'getTenantDetail')")
    public TenantResponseDto detail(@PathVariable @NotBlank String id) {
        return tenantService.detail(id);
    }

    @Operation(summary = "删除租户", description = "删除租户")
    @Parameters({
            @Parameter(name = "id", description = "租户 ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/{id}")
    @TenantLimit("master")
    @PreAuthorize("@pms.hasAnyPermission('allTenantPermissions', 'deleteTenant')")
    public void removeTenant(@PathVariable @NotBlank String id) {
        tenantService.removeTenant(id);
    }

    @Operation(summary = "检查租户", description = "检查租户")
    @Parameters({
            @Parameter(name = "code", description = "租户标识", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/check/{code}")
    public CheckTenantResponseDto checkTenant(@PathVariable @NotBlank String code) {
        return tenantService.checkTenant(code);
    }
}
