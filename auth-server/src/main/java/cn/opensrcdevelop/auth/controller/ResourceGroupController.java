package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.resource.ResourceResponseDto;
import cn.opensrcdevelop.auth.biz.dto.resource.group.ResourceGroupRequestDto;
import cn.opensrcdevelop.auth.biz.dto.resource.group.ResourceGroupResponseDto;
import cn.opensrcdevelop.auth.biz.service.ResourceGroupService;
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

@Tag(name = "API-ResourceGroup", description = "接口-资源组管理")
@RestController
@RestResponse
@RequestMapping("/resourceGroup")
@RequiredArgsConstructor
public class ResourceGroupController {

    private final ResourceGroupService resourceGroupService;

    @Operation(summary = "获取资源组列表", description = "获取资源组列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "资源组名称 / 标识检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/list")
    @Authorize({ "allResourceGroupPermissions", "listResourceGroup" })
    public PageData<ResourceGroupResponseDto> list(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return resourceGroupService.list(page, size, keyword);
    }

    @Operation(summary = "获取组内资源", description = "获取组内资源")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "id", description = "资源组ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "keyword", description = "资源名称 / 标识检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/{id}/resources")
    @Authorize({ "allResourceGroupPermissions", "getResourceGroupResources" })
    public PageData<ResourceResponseDto> getGroupResources(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @PathVariable String id, @RequestParam(required = false) String keyword) {
        return resourceGroupService.getGroupResources(page, size, id, keyword);
    }

    @Operation(summary = "创建资源组", description = "创建资源组")
    @PostMapping
    @Authorize({ "allResourceGroupPermissions", "createResourceGroup" })
    public void createResourceGroup(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class })ResourceGroupRequestDto requestDto) {
        resourceGroupService.createResourceGroup(requestDto);
    }

    @Operation(summary = "更新资源组", description = "更新资源组")
    @PutMapping
    @Authorize({ "allResourceGroupPermissions", "updateResourceGroup" })
    public void updateResourceGroup(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) ResourceGroupRequestDto requestDto) {
        resourceGroupService.updateResourceGroup(requestDto);
    }

    @Operation(summary = "删除资源组", description = "删除资源组")
    @Parameters({
            @Parameter(name = "id", description = "资源组ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/{id}")
    @Authorize({ "allResourceGroupPermissions", "deleteResourceGroup" })
    public void removeResourceGroup(@PathVariable @NotBlank String id) {
        resourceGroupService.removeResourceGroup(List.of(id));
    }

    @Operation(summary = "获取资源组详情", description = "获取资源组详情")
    @Parameters({
            @Parameter(name = "id", description = "资源组ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/{id}")
    @Authorize({ "allResourceGroupPermissions", "getResourceGroupDetail" })
    public ResourceGroupResponseDto detail(@PathVariable @NotBlank String id) {
        return resourceGroupService.detail(id);
    }
}
