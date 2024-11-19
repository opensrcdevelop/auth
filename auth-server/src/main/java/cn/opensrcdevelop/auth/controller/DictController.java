package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.DictDataRequestDto;
import cn.opensrcdevelop.auth.biz.dto.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.dto.DictRequestDto;
import cn.opensrcdevelop.auth.biz.dto.DictResponseDto;
import cn.opensrcdevelop.auth.biz.service.DictDataService;
import cn.opensrcdevelop.auth.biz.service.DictService;
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

@Tag(name = "API-Dict", description = "接口-数据字典")
@RestController
@RestResponse
@RequestMapping("/dict")
@RequiredArgsConstructor
public class DictController {

    private final DictService dictService;
    private final DictDataService dictDataService;

    @Operation(summary = "创建字典", description = "创建字典")
    @PostMapping
    @Authorize({"allDictPermissions", "createDict"})
    public void createDict(@RequestBody @Validated({ValidationGroups.Operation.INSERT.class}) DictRequestDto requestDto) {
        dictService.createDict(requestDto);
    }

    @Operation(summary = "更新字典", description = "更新字典")
    @PutMapping
    @Authorize({"allDictPermissions", "updateDict"})
    public void updateDict(@RequestBody @Validated({ValidationGroups.Operation.UPDATE.class}) DictRequestDto requestDto) {
        dictService.updateDict(requestDto);
    }

    @Operation(summary = "获取字典详情", description = "获取字典详情")
    @Parameters({
            @Parameter(name = "id", description = "字典ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/{id}")
    @Authorize({"allDictPermissions", "getDictDetail"})
    public DictResponseDto detail(@PathVariable @NotBlank String id) {
        return dictService.detail(id);
    }

    @Operation(summary = "获取字典列表", description = "获取字典列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "字典名称或标识检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/list")
    @Authorize({"allDictPermissions", "listDict"})
    public PageData<DictResponseDto> list(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return dictService.list(page, size, keyword);
    }

    @Operation(summary = "删除字典", description = "删除字典")
    @Parameters({
            @Parameter(name = "id", description = "字典ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/{id}")
    @Authorize({"allDictPermissions", "deleteDict"})
    public void removeDict(@PathVariable @NotBlank String id) {
        dictService.removeDict(id);
    }

    @Operation(summary = "创建字典数据", description = "创建字典数据")
    @PostMapping("/data")
    @Authorize({"allDictDataPermissions", "createDictData"})
    public void createDictData(@RequestBody @Validated({ValidationGroups.Operation.INSERT.class}) DictDataRequestDto requestDto) {
        dictDataService.createDictData(requestDto);
    }

    @Operation(summary = "更新字典数据", description = "更新字典数据")
    @PutMapping("/data")
    @Authorize({"allDictDataPermissions", "updateDictData"})
    public void updateDictData(@RequestBody @Validated({ValidationGroups.Operation.UPDATE.class}) DictDataRequestDto requestDto) {
        dictDataService.updateDictData(requestDto);
    }

    @Operation(summary = "获取字典数据详情", description = "获取字典数据详情")
    @Parameters({
            @Parameter(name = "id", description = "字典数据ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/data/{id}")
    @Authorize({"allDictDataPermissions", "getDictDataDetail"})
    public DictDataResponseDto dictDataDetail(@PathVariable @NotBlank String id) {
        return dictDataService.detail(id);
    }

    @Operation(summary = "获取字典数据列表", description = "获取字典数据列表")
    @Parameters({
            @Parameter(name = "id", description = "字典ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "字典数据标签或值检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/{id}/data/list")
    @Authorize({"allDictDataPermissions", "listDictData"})
    public PageData<DictDataResponseDto> dictDataList(@PathVariable @NotBlank String id, @RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return dictDataService.list(id, page, size, keyword);
    }

    @Operation(summary = "删除字典数据", description = "删除字典数据")
    @Parameters({
            @Parameter(name = "id", description = "字典数据ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/data/{id}")
    @Authorize({"allDictDataPermissions", "deleteDictData"})
    public void removeDictData(@PathVariable @NotBlank String id) {
        dictDataService.removeDictData(List.of(id));
    }

    @Operation(summary = "获取启用的字典数据", description = "获取启用的字典数据")
    @Parameters({
            @Parameter(name = "id", description = "字典ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/{id}/data/enabled")
    public List<DictDataResponseDto> getEnabledDictData(@PathVariable @NotBlank String id) {
        return dictDataService.getEnabledDictData(id);
    }
}
