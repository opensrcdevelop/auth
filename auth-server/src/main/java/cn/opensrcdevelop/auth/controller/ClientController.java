package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.ClientRequestDto;
import cn.opensrcdevelop.auth.biz.dto.ClientResponseDto;
import cn.opensrcdevelop.auth.biz.dto.CreateOrUpdateSecretClientResponseDto;
import cn.opensrcdevelop.auth.biz.service.impl.ClientServiceImpl;
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
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "API-Client", description = "接口-客户端管理")
@RestController
@RestResponse
@RequestMapping("/client")
@RequiredArgsConstructor
public class ClientController {

    private final ClientServiceImpl clientService;

    @Operation(summary = "创建客户端", description = "创建客户端")
    @PostMapping
    @PreAuthorize("@pms.hasAnyPermission('allClientPermissions', 'createClient')")
    public CreateOrUpdateSecretClientResponseDto createClient(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) ClientRequestDto requestDto) {
        return clientService.createClient(requestDto);
    }

    @Operation(summary = "获取客户端列表", description = "获取客户端列表")
    @Parameters({
            @Parameter(name = "keyword", description = "名称关键字", in = ParameterIn.QUERY),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true)
    })
    @GetMapping("/list")
    @PreAuthorize("@pms.hasAnyPermission('allClientPermissions', 'listClient')")
    public PageData<ClientResponseDto> list(@RequestParam(required = false) String keyword, @RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "20") int size) {
        return clientService.list(keyword, page, size);
    }

    @Operation(summary = "获取客户端详情", description = "获取客户端详情")
    @Parameters({
            @Parameter(name = "id", description = "客户端ID", in = ParameterIn.PATH, required = true),
    })
    @GetMapping("/{id}")
    @PreAuthorize("@pms.hasAnyPermission('allClientPermissions', 'getClientDetail')")
    public ClientResponseDto details(@PathVariable @NotBlank String id) {
        return clientService.details(id);
    }

    @Operation(summary = "更新客户端", description = "更新客户端")
    @PutMapping
    @PreAuthorize("@pms.hasAnyPermission('allClientPermissions', 'updateClient')")
    public void updateClient(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) ClientRequestDto requestDto) {
        clientService.updateClient(requestDto);
    }

    @Operation(summary = "更新客户端密钥", description = "更新客户端密钥")
    @Parameters({
            @Parameter(name = "id", description = "客户端ID", in = ParameterIn.PATH, required = true),
    })
    @PutMapping("/secret/{id}")
    @PreAuthorize("@pms.hasAnyPermission('allClientPermissions', 'updateClientSecret')")
    public CreateOrUpdateSecretClientResponseDto updateClientSecret(@PathVariable @NotBlank String id) {
        return clientService.updateClientSecret(id);
    }

    @Operation(summary = "删除客户端", description = "删除客户端")
    @Parameters({
            @Parameter(name = "id", description = "客户端ID", in = ParameterIn.PATH, required = true),
    })
    @DeleteMapping("/{id}")
    @PreAuthorize("@pms.hasAnyPermission('allClientPermissions', 'deleteClient')")
    public void deleteClient(@PathVariable @NotBlank String id) {
        clientService.deleteClient(id);
    }
}
