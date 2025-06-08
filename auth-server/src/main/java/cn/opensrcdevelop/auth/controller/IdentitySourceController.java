package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.identity.*;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceProviderService;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.auth.biz.service.identity.ThirdAccountService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
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
    @Authorize({ "allIdentitySourceProviderPermissions", "listIdentitySourceProvider" })
    public PageData<IdentitySourceProviderResponseDto> listProviders(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return identitySourceProviderService.list(page, size, keyword);
    }

    @Operation(summary = "获取身份源提供商详情", description = "获取身份源提供商详情")
    @Parameters({
            @Parameter(name = "id", description = "身份源提供商ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/provider/{id}")
    @Authorize({ "allIdentitySourceProviderPermissions", "getIdentitySourceProviderDetail" })
    public IdentitySourceProviderResponseDto getProviderDetail(@PathVariable @NotBlank String id) {
        return identitySourceProviderService.detail(id);
    }

    @Operation(summary = "创建身份源提供商", description = "创建身份源提供商")
    @PostMapping("/provider")
    @Authorize({ "allIdentitySourceProviderPermissions", "createIdentitySourceProvider" })
    public void createProvider(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) IdentitySourceProviderRequestDto requestDto) {
        identitySourceProviderService.createIdentitySourceProvider(requestDto);
    }

    @Operation(summary = "更新身份源提供商", description = "更新身份源提供商")
    @PutMapping("/provider")
    @Authorize({ "allIdentitySourceProviderPermissions", "updateIdentitySourceProvider" })
    public void updateProvider(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) IdentitySourceProviderRequestDto requestDto) {
        identitySourceProviderService.updateIdentitySourceProvider(requestDto);
    }

    @Operation(summary = "删除身份源提供商", description = "删除身份源提供商")
    @Parameters({
            @Parameter(name = "id", description = "身份源提供商ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/provider/{id}")
    @Authorize({ "allIdentitySourceProviderPermissions", "deleteIdentitySourceProvider" })
    public void deleteProvider(@PathVariable @NotBlank String id) {
        identitySourceProviderService.removeIdentitySourceProvider(id);
    }

    @Operation(summary = "获取身份源提供商关联的身份源列表", description = "获取身份源提供商关联的身份源列表")
    @Parameters({
            @Parameter(name = "id", description = "身份源提供商ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/provider/{id}/registrations")
    @Authorize({ "allIdentitySourceProviderPermissions", "getIdentitySourceProviderRegistrations" })
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
    @Authorize({ "allIdentitySourcePermissions", "listIdentitySource" })
    public PageData<IdentitySourceRegistrationResponseDto> listRegistrations(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return identitySourceRegistrationService.list(page, size, keyword);
    }

    @Operation(summary = "获取身份源详情", description = "获取身份源详情")
    @Parameters({
            @Parameter(name = "id", description = "身份源ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/{id}")
    @Authorize({ "allIdentitySourcePermissions", "getIdentitySourceDetail" })
    public IdentitySourceRegistrationResponseDto getRegistrationDetail(@PathVariable @NotBlank String id) {
        return identitySourceRegistrationService.detail(id);
    }

    @Operation(summary = "创建身份源", description = "创建身份源")
    @PostMapping
    @Authorize({ "allIdentitySourcePermissions", "createIdentitySource" })
    public void createRegistration(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) IdentitySourceRegistrationRequestDto requestDto) {
        identitySourceRegistrationService.createIdentitySourceRegistration(requestDto);
    }

    @Operation(summary = "更新身份源", description = "更新身份源")
    @PutMapping
    @Authorize({ "allIdentitySourcePermissions", "updateIdentitySource" })
    public void updateRegistration(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) IdentitySourceRegistrationRequestDto requestDto) {
        identitySourceRegistrationService.updateIdentitySourceRegistration(requestDto);
    }

    @Operation(summary = "删除身份源", description = "删除身份源")
    @Parameters({
            @Parameter(name = "id", description = "身份源ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/{id}")
    @Authorize({ "allIdentitySourcePermissions", "deleteIdentitySource" })
    public void deleteRegistration(@PathVariable @NotBlank String id) {
        identitySourceRegistrationService.removeIdentitySourceRegistration(id);
    }

    @Operation(summary = "获取启用的身份源", description = "获取启用的身份源")
    @GetMapping("/enabled")
    public List<IdentitySourceRegistrationResponseDto> getEnabledRegistrations() {
        return identitySourceRegistrationService.getEnabledRegistrations();
    }

    @Operation(summary = "获取身份源关联的用户绑定列表", description = "获取身份源关联的用户绑定列表")
    @Parameters({
            @Parameter(name = "id", description = "身份源ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "用户名检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/{id}/userBindings")
    @Authorize({ "allIdentitySourcePermissions", "getIdentitySourceUserBindings" })
    public PageData<UserBindingResponseDto> getUserBindingList(@PathVariable @NotBlank String id, @RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestParam(required = false) String keyword) {
        return thirdAccountService.getUserBindingList(id, page, size, keyword);
    }

    @Operation(summary = "获取当前用户已绑定的身份源", description = "获取当前用户已绑定的身份源")
    @GetMapping("/bound")
    public List<IdentitySourceRegistrationResponseDto> getBoundRegistrationIds() {
        return identitySourceRegistrationService.getBoundRegistrations();
    }

    @Operation(summary = "绑定当前用户", description = "绑定当前用户")
    @Parameters({
            @Parameter(name = "code", description = "身份源标识", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/bind/{code}")
    public UserBindingResponseDto bindUser(@PathVariable @NotBlank String code, HttpServletRequest request, HttpServletResponse response) throws IOException {
        return identitySourceRegistrationService.bindUser(code, request, response);
    }

    @Operation(summary = "解绑当前用户", description = "解绑当前用户")
    @Parameters({
            @Parameter(name = "id", description = "身份源ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/{id}/unbind")
    public void unbindUser(@PathVariable @NotBlank String id) {
        identitySourceRegistrationService.unbindUser(id);
    }
}
