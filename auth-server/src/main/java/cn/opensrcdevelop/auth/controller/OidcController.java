package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcClaimRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcClaimResponseDto;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcScopeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.oidc.OidcScopeResponseDto;
import cn.opensrcdevelop.auth.biz.service.client.oidc.OidcClaimService;
import cn.opensrcdevelop.auth.biz.service.client.oidc.OidcScopeService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
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

@Tag(name = "API-OIDC", description = "接口-OIDC")
@RestController
@RestResponse
@RequestMapping("/oidc")
@RequiredArgsConstructor
public class OidcController {

    private final OidcScopeService oidcScopeService;
    private final OidcClaimService oidcClaimService;


    @Operation(summary = "创建 OIDC scope", description = "创建 OIDC scope")
    @PostMapping("/scope")
    @Authorize({ "allOidcScopePermissions", "createOidcScope" })
    public void createOidcScope(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) OidcScopeRequestDto requestDto) {
        oidcScopeService.createOidcScope(requestDto);
    }

    @Operation(summary = "创建 OIDC claim", description = "创建 OIDC claim")
    @PostMapping("/claim")
    @Authorize({ "allOidcClaimPermissions", "createOidcClaim" })
    public void createOidcClaim(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) OidcClaimRequestDto requestDto) {
        oidcClaimService.createOidcClaim(requestDto);
    }

    @Operation(summary = "获取所有 OIDC scope", description = "获取所有 OIDC scope")
    @GetMapping("/scope/list")
    @Authorize({ "allOidcScopePermissions", "listOidcScope" })
    public List<OidcScopeResponseDto> listScopes() {
        return oidcScopeService.listScopes();
    }

    @Operation(summary = "获取所有 OIDC claim", description = "获取所有 OIDC claim")
    @GetMapping("/claim/list")
    @Authorize({ "allOidcClaimPermissions", "listOidcClaim" })
    public List<OidcClaimResponseDto> listClaims() {
        return oidcClaimService.listClaims();
    }

    @Operation(summary = "更新 OIDC scope", description = "更新 OIDC scope")
    @PutMapping("/scope")
    @Authorize({ "allOidcScopePermissions", "updateOidcScope" })
    public void updateOidcScope(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) OidcScopeRequestDto requestDto) {
        oidcScopeService.updateOidcScope(requestDto);
    }

    @Operation(summary = "删除 OIDC scope", description = "删除 OIDC scope")
    @Parameters({
            @Parameter(name = "id", description = "scope ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/scope/{id}")
    @Authorize({ "allOidcScopePermissions", "deleteOidcScope" })
    public void removeOidcScope(@PathVariable @NotBlank String id) {
        oidcScopeService.removeOidcScope(id);
    }

    @Operation(summary = "更新 OIDC claim", description = "更新 OIDC claim")
    @PutMapping("/claim")
    @Authorize({ "allOidcClaimPermissions", "updateOidcClaim" })
    public void updateOidcClaim(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) OidcClaimRequestDto requestDto) {
        oidcClaimService.updateOidcClaim(requestDto);
    }

    @Operation(summary = "删除 OIDC claim", description = "删除 OIDC claim")
    @Parameters({
            @Parameter(name = "id", description = "claim ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/claim/{id}")
    @Authorize({ "allOidcClaimPermissions", "deleteOidcClaim" })
    public void removeOidcClaim(@PathVariable @NotBlank String id) {
        oidcClaimService.removeOidcClaim(id);
    }
}
