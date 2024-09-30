package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.annocation.ResourceLimit;
import cn.opensrcdevelop.auth.biz.dto.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.ResourceRequestDto;
import cn.opensrcdevelop.auth.biz.dto.ResourceResponseDto;
import cn.opensrcdevelop.auth.biz.service.ResourceService;
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
    @ResourceLimit(ids = { "c0b4ee30-bf40-4299-9fab-ff32328b047a" }, idEl = "#requestDto.resourceGroupId")
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

    }, idEl = "#requestDto.id")
    public void updateResource(@RequestBody @Validated(ValidationGroups.Operation.UPDATE.class) ResourceRequestDto requestDto) {
        resourceService.updateResource(requestDto);
    }

    @Operation(summary = "删除资源", description = "删除资源")
    @Parameters({
            @Parameter(name = "id", description = "资源ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/{id}")
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

    }, idEl = "#id")
    @Authorize({ "allResourcePermissions", "deleteResource" })
    public void removeResource(@PathVariable @NotBlank String id) {
        resourceService.removeResource(List.of(id));
    }
}
