package cn.opensrcdevelop.auth.controller.user;

import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestCreateDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestListItemDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestResponseDto;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.response.PageData;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
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

    @Operation(summary = "获取我的权限申请记录列表", description = "返回当前用户的权限申请记录列表，分页展示，按申请时间倒序排列")
    @GetMapping
    public PageData<PermissionRequestListItemDto> listRequests(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "15") int size) {
        return permissionRequestService.listUserRequests(page, size);
    }
}
