package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.identity.*;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceProviderService;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.auth.biz.service.identity.ThirdAccountService;
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

@Tag(name = "API-IdentitySource", description = "接口-身份源管理")
@RestController
@RestResponse
@RequestMapping("/identitySource")
@RequiredArgsConstructor
public class IdentitySourceController {

    private final IdentitySourceProviderService identitySourceProviderService;
    private final IdentitySourceRegistrationService identitySourceRegistrationService;
    private final ThirdAccountService thirdAccountService;

    @Operation(summary = "获取身份源提供商列表", description = "获取身份源提供商列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "身份源提供商名称或标识检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/provider/list")
    public PageData<IdentitySourceProviderResponseDto> listProviders(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return identitySourceProviderService.list(page, size, keyword);
    }

    @Operation(summary = "获取身份源提供商详情", description = "获取身份源提供商详情")
    @Parameters({
            @Parameter(name = "id", description = "身份源提供商ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/provider/{id}")
    public IdentitySourceProviderResponseDto getProviderDetail(@PathVariable @NotBlank String id) {
        return identitySourceProviderService.detail(id);
    }

    @Operation(summary = "创建身份源提供商", description = "创建身份源提供商")
    @PostMapping("/provider")
    public void createProvider(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) IdentitySourceProviderRequestDto requestDto) {
        identitySourceProviderService.createIdentitySourceProvider(requestDto);
    }

    @Operation(summary = "更新身份源提供商", description = "更新身份源提供商")
    @PutMapping("/provider")
    public void updateProvider(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) IdentitySourceProviderRequestDto requestDto) {
        identitySourceProviderService.updateIdentitySourceProvider(requestDto);
    }

    @Operation(summary = "删除身份源提供商", description = "删除身份源提供商")
    @Parameters({
            @Parameter(name = "id", description = "身份源提供商ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/provider/{id}")
    public void deleteProvider(@PathVariable @NotBlank String id) {
        identitySourceProviderService.removeIdentitySourceProvider(id);
    }

    @Operation(summary = "获取关联的身份源列表", description = "获取关联的身份源列表")
    @Parameters({
            @Parameter(name = "id", description = "身份源提供商ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/provider/{id}/registrations")
    public List<IdentitySourceRegistrationResponseDto> registrations(@PathVariable @NotBlank String id) {
        return identitySourceProviderService.registrations(id);
    }

    @Operation(summary = "获取身份源列表", description = "获取身份源列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "获取身份源注册名称或标识检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/list")
    public PageData<IdentitySourceRegistrationResponseDto> listRegistrations(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return identitySourceRegistrationService.list(page, size, keyword);
    }

    @Operation(summary = "获取身份源详情", description = "获取身份源详情")
    @Parameters({
            @Parameter(name = "id", description = "身份源ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/{id}")
    public IdentitySourceRegistrationResponseDto getRegistrationDetail(@PathVariable @NotBlank String id) {
        return identitySourceRegistrationService.detail(id);
    }

    @Operation(summary = "注册身份源", description = "注册身份源")
    @PostMapping
    public void createRegistration(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) IdentitySourceRegistrationRequestDto requestDto) {
        identitySourceRegistrationService.createIdentitySourceRegistration(requestDto);
    }

    @Operation(summary = "更新身份源", description = "更新身份源")
    @PutMapping
    public void updateRegistration(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) IdentitySourceRegistrationRequestDto requestDto) {
        identitySourceRegistrationService.updateIdentitySourceRegistration(requestDto);
    }

    @Operation(summary = "删除身份源", description = "删除身份源")
    @Parameters({
            @Parameter(name = "id", description = "身份源ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/{id}")
    public void deleteRegistration(@PathVariable @NotBlank String id) {
        identitySourceRegistrationService.removeIdentitySourceRegistration(id);
    }

    @Operation(summary = "获取启用的身份源", description = "获取启用的身份源")
    @GetMapping("/enabled")
    public List<IdentitySourceRegistrationResponseDto> getEnabledRegistrations() {
        return identitySourceRegistrationService.getEnabledRegistrations();
    }

    @Operation(summary = "获取用户绑定列表", description = "获取用户绑定列表")
    @Parameters({
            @Parameter(name = "id", description = "注册身份源ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "用户名检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/{id}/userBindings")
    public PageData<UserBindingResponseDto> getUserBindingList(@PathVariable @NotBlank String id, @RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return thirdAccountService.getUserBindingList(id, page, size, keyword);
    }
}
