package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.resource.ResourceRequestDto;
import cn.opensrcdevelop.auth.biz.dto.resource.ResourceResponseDto;
import cn.opensrcdevelop.auth.biz.service.resource.ResourceService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "API-Resource", description = "接口-资源管理")
@RestController
@RestResponse
@RequestMapping("/resource")
@RequiredArgsConstructor
public class ResourceController {

    private final ResourceService resourceService;

    @Operation(summary = "创建资源", description = "创建资源")
    @PostMapping
    @Authorize({ "allResourcePermissions", "createResource" })
    public void createResource(@RequestBody @Validated(ValidationGroups.Operation.INSERT.class) ResourceRequestDto requestDto) {
        resourceService.createResource(requestDto);
    }

    @Operation(summary = "获取资源列表", description = "获取资源列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "资源名称 / 标识检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/list")
    @Authorize({ "allResourcePermissions", "listResource" })
    public PageData<ResourceResponseDto> list(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return resourceService.list(page, size, keyword);
    }

    @Operation(summary = "获取资源详情", description = "获取资源详情")
    @Parameters({
            @Parameter(name = "id", description = "资源ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/{id}")
    @Authorize({ "allResourcePermissions", "getResourceDetail" })
    public ResourceResponseDto detail(@PathVariable @NotBlank String id) {
        return resourceService.detail(id);
    }

    @Operation(summary = "获取资源内权限", description = "获取资源内权限")
    @Parameters({
            @Parameter(name = "id", description = "资源ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "权限名称 / 标识检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/{id}/permissions")
    @Authorize({ "allResourcePermissions", "getResourcePermissions" })
    public PageData<PermissionResponseDto> getResourcePermissions(@PathVariable String id, @RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return resourceService.getResourcePermissions(page, size, id, keyword);
    }

    @Operation(summary = "更新资源", description = "更新资源")
    @PutMapping
    @Authorize({ "allResourcePermissions", "updateResource" })
    public void updateResource(@RequestBody @Validated(ValidationGroups.Operation.UPDATE.class) ResourceRequestDto requestDto) {
        resourceService.updateResource(requestDto);
    }

    @Operation(summary = "删除资源", description = "删除资源")
    @Parameters({
            @Parameter(name = "id", description = "资源ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/{id}")
    @Authorize({ "allResourcePermissions", "deleteResource" })
    public void removeResource(@PathVariable @NotBlank String id) {
        resourceService.removeResource(List.of(id));
    }
}
