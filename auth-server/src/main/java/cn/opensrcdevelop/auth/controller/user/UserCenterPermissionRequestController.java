package cn.opensrcdevelop.auth.controller.user;

import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestCreateDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestResponseDto;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "API-UserCenter-PermissionRequest", description = "接口-用户中心-权限申请")
@RestController
@RestResponse
@RequestMapping("/user-center/permissions/requests")
@RequiredArgsConstructor
public class UserCenterPermissionRequestController {

    private final PermissionRequestService permissionRequestService;

    @Operation(summary = "提交权限申请", description = "用户提交权限申请，支持批量申请多个权限，共用一个申请理由")
    @PostMapping
    @Authorize({"submitPermissionRequest"})
    public PermissionRequestResponseDto submitRequest(@RequestBody @Valid PermissionRequestCreateDto requestDto) {
        return permissionRequestService.submitRequest(requestDto);
    }
}
