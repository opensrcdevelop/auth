package cn.opensrcdevelop.auth.controller.admin;

import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestListItemDto;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestAdminService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.response.PageData;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "API-Admin-PermissionRequest", description = "接口-管理员-权限申请")
@RestController
@RestResponse
@RequestMapping("/admin/permissions/requests")
@RequiredArgsConstructor
public class AdminPermissionRequestController {

    private final PermissionRequestAdminService permissionRequestAdminService;

    @Operation(summary = "获取待审批权限申请列表", description = "返回PENDING和AUTO_APPROVED状态的申请，分页展示，按申请时间倒序")
    @GetMapping("/pending")
    @Authorize({"allPermissionRequestPermissions", "listPendingPermissionRequests"})
    public PageData<PermissionRequestListItemDto> listPendingRequests(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "15") int size) {
        return permissionRequestAdminService.listPendingRequests(page, size);
    }

    @Operation(summary = "获取所有权限申请列表", description = "返回所有申请记录，支持按状态筛选，分页展示，按申请时间倒序")
    @GetMapping("/all")
    @Authorize({"allPermissionRequestPermissions", "listAllPermissionRequests"})
    public PageData<PermissionRequestListItemDto> listAllRequests(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "15") int size,
            @RequestParam(required = false) String status) {
        return permissionRequestAdminService.listAllRequests(page, size, status);
    }
}
