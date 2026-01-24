package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.*;
import cn.opensrcdevelop.auth.biz.dto.user.attr.SetUserAttrDisplaySeqRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportResultDto;
import cn.opensrcdevelop.auth.biz.service.user.LoginLogService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrService;
import cn.opensrcdevelop.auth.biz.service.user.excel.UserExcelService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.NoRestResponse;
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
import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;

@Tag(name = "API-User", description = "接口-用户管理")
@RestController
@RestResponse
@RequestMapping("/user")
@RequiredArgsConstructor
public class UserController {

    private final UserService userService;
    private final UserAttrService userAttrService;
    private final LoginLogService loginLogService;
    private final UserExcelService userExcelService;

    @Operation(summary = "创建用户", description = "创建用户")
    @PostMapping
    @Authorize({"allUserPermissions", "createUser"})
    public void createUser(
            @RequestBody @Validated({ValidationGroups.Operation.INSERT.class}) UserRequestDto requestDto) {
        userService.createUser(requestDto);
    }

    @Operation(summary = "创建用户属性", description = "创建用户属性")
    @PostMapping("/attr")
    @Authorize({"allUserAttrPermissions", "createUserAttr"})
    public void createUserAttr(
            @RequestBody @Validated({ValidationGroups.Operation.INSERT.class}) UserAttrRequestDto requestDto) {
        userAttrService.createUserAttr(requestDto);
    }

    @Operation(summary = "获取所有用户属性", description = "获取所有用户属性")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "onlyDisplay", description = "返回只在用户列表显示的用户属性", in = ParameterIn.QUERY),
            @Parameter(name = "keyword", description = "用户属性名称或 key 检索关键字", in = ParameterIn.QUERY)
    })
    @GetMapping("/attr/list")
    @Authorize({"allUserAttrPermissions", "listUserAttr"})
    public PageData<UserAttrResponseDto> listUserAttrs(@RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "15") int size,
            @RequestParam(required = false, defaultValue = "false") Boolean onlyDisplay,
            @RequestParam(required = false) String keyword) {
        return userAttrService.listUserAttrs(page, size, onlyDisplay, keyword);
    }

    @Operation(summary = "获取用户列表", description = "获取用户列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true)
    })
    @PostMapping("/list")
    @Authorize({"allUserPermissions", "listUser"})
    public PageData<Map<String, Object>> list(@RequestParam(defaultValue = "1") @Min(1) int page,
            @RequestParam(defaultValue = "15") int size, @RequestBody @Valid List<DataFilterDto> filters) {
        return userService.list(page, size, filters);
    }

    @Operation(summary = "更新用户信息", description = "更新用户信息")
    @PutMapping
    @Authorize({"allUserPermissions", "updateUser"})
    public void updateUser(
            @RequestBody @Validated({ValidationGroups.Operation.UPDATE.class}) UserRequestDto requestDto) {
        userService.updateUser(requestDto);
    }

    @Operation(summary = "更新用户属性", description = "更新用户属性")
    @PutMapping("/attr")
    @Authorize({"allUserAttrPermissions", "updateUserAttr"})
    public void updateUserAttr(
            @RequestBody @Validated({ValidationGroups.Operation.UPDATE.class}) UserAttrRequestDto requestDto) {
        userAttrService.updateUserAttr(requestDto);
    }

    @Operation(summary = "设置用户属性显示顺序", description = "设置用户属性显示顺序")
    @PostMapping("/attr/seq")
    @Authorize({"allUserAttrPermissions", "setUserAttrDisplaySeq"})
    public void setUserAttrDisplaySeq(
            @RequestBody @NotEmpty @Valid List<SetUserAttrDisplaySeqRequestDto> requestDtoList) {
        userAttrService.setUserAttrDisplaySeq(requestDtoList);
    }

    @Operation(summary = "获取用户详情", description = "获取用户详情")
    @Parameters({
            @Parameter(name = "id", description = "用户ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/{id}")
    @Authorize({"allUserPermissions", "getUserDetail"})
    public UserResponseDto userDetail(@PathVariable @NotBlank String id) {
        return userService.detail(id);
    }

    @Operation(summary = "变更密码", description = "变更密码")
    @PostMapping("/me/password/change")
    public void changePwd(@RequestBody @Valid ChangePwdRequestDto requestDto, HttpServletRequest request) {
        userService.changePwd(requestDto, request);
    }

    @Operation(summary = "删除用户", description = "删除用户")
    @Parameters({
            @Parameter(name = "id", description = "用户ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/{id}")
    @Authorize({"allUserPermissions", "deleteUser"})
    public void removeUser(@PathVariable @NotBlank String id) {
        userService.removeUser(id);
    }

    @Operation(summary = "获取用户属性详情", description = "获取用户属性详情")
    @Parameters({
            @Parameter(name = "id", description = "用户属性ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/attr/{id}")
    @Authorize({"allUserAttrPermissions", "getUserAttrDetail"})
    public UserAttrResponseDto userAttrDetail(@PathVariable @NotBlank String id) {
        return userAttrService.detail(id);
    }

    @Operation(summary = "删除用户属性", description = "删除用户属性")
    @DeleteMapping("/attr/{id}")
    @Authorize({"allUserAttrPermissions", "deleteUserAttr"})
    public void removeUserAttr(@PathVariable @NotBlank String id) {
        userAttrService.removeUserAttr(id);
    }

    @Operation(summary = "获取当前用户信息", description = "获取当前用户信息")
    @GetMapping
    public Map<String, Object> getCurrentUserInfo() {
        return userService.getCurrentUserInfo();
    }

    @Operation(summary = "重新绑定 MFA 设备", description = "重新绑定 MFA 设备")
    @Parameters({
            @Parameter(name = "id", description = "用户ID", in = ParameterIn.PATH, required = true)
    })
    @PutMapping("/{id}/mfa/device")
    @Authorize({"allUserPermissions", "rebindMfaDevice"})
    public void rebindMfaDevice(@PathVariable @NotBlank String id) {
        userService.rebindMfaDevice(id);
    }

    @Operation(summary = "清空授权的 Token", description = "清空授权的 Token")
    @Parameters({
            @Parameter(name = "id", description = "用户ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/{id}/token")
    @Authorize({"allUserPermissions", "clearTokens"})
    public void clearAuthorizedTokens(@PathVariable @NotBlank String id) {
        userService.clearAuthorizedTokens(id);
    }

    @Operation(summary = "重置密码", description = "重置密码")
    @PostMapping("/me/password/reset")
    public void resetPwd(@RequestBody @Valid ResetPwdRequestDto requestDto) {
        userService.resetPwd(requestDto);
    }

    @Operation(summary = "获取个人中心可见的用户属性", description = "获取个人中心可见的用户属性")
    @GetMapping("/attr/list/visible")
    public List<UserAttrResponseDto> getVisibleUserAttrs() {
        return userAttrService.getVisibleUserAttrs();
    }

    @Operation(summary = "更新个人信息", description = "更新个人信息")
    @PutMapping("/me")
    public void updateMe(@RequestBody Map<String, Object> userInfo) {
        userService.updateMe(userInfo);
    }

    @Operation(summary = "绑定邮箱", description = "绑定邮箱")
    @PostMapping("/me/email/bind")
    public void bindEmail(@RequestBody @Valid BindOrUnbindEmailRequestDto requestDto) {
        userService.bindEmail(requestDto);
    }

    @Operation(summary = "解绑邮箱", description = "解绑邮箱")
    @PostMapping("/me/email/unbind")
    public void unbindEmail(@RequestBody @Valid BindOrUnbindEmailRequestDto requestDto) {
        userService.unbindEmail(requestDto);
    }

    @Operation(summary = "获取用户权限", description = "获取用户权限")
    @Parameters({
            @Parameter(name = "id", description = "用户ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "resourceGroupNameSearchKeyword", description = "资源组名称检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "resourceNameSearchKeyword", description = "资源名称检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "permissionNameSearchKeyword", description = "权限名称检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "permissionCodeSearchKeyword", description = "权限标识检索关键字", in = ParameterIn.QUERY),
    })
    @GetMapping("/{id}/permissions")
    @Authorize({"allUserPermissions", "getUserPermissions"})
    public PageData<PermissionResponseDto> getPermissions(@RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "15") int size, @PathVariable @NotBlank String id,
            @RequestParam(required = false) String resourceGroupNameSearchKeyword,
            @RequestParam(required = false) String resourceNameSearchKeyword,
            @RequestParam(required = false) String permissionNameSearchKeyword,
            @RequestParam(required = false) String permissionCodeSearchKeyword) {
        return userService.getPermissions(page, size, id, resourceGroupNameSearchKeyword, resourceNameSearchKeyword,
                permissionNameSearchKeyword, permissionCodeSearchKeyword);
    }

    @Operation(summary = "获取用户登录日志", description = "获取用户登录日志")
    @Parameters({
            @Parameter(name = "id", description = "用户ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true)
    })
    @GetMapping("/{id}/loginLogs")
    @Authorize({"allUserPermissions", "getUserLoginLogs"})
    public PageData<LoginLogResponseDto> getUserLoginLogs(@RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "15") int size, @PathVariable @NotBlank String id) {
        return loginLogService.getUserLoginLogs(page, size, id);
    }

    @Operation(summary = "根据登录ID清空授权的 Token", description = "根据登录ID清空授权的 Token")
    @Parameters({
            @Parameter(name = "id", description = "登录ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/login/{id}/token")
    @Authorize({"allUserPermissions", "clearTokensByLoginId"})
    public void clearAuthorizedTokensByLoginId(@PathVariable @NotBlank String id) {
        userService.clearAuthorizedTokensByLoginId(id);
    }

    @Operation(summary = "下载用户导入模版", description = "下载用户导入模版")
    @GetMapping("/excel/template")
    @Authorize({"allUserPermissions", "exportUser"})
    @NoRestResponse
    public void downloadImportTemplate(HttpServletResponse response) throws IOException {
        byte[] template = userExcelService.generateImportTemplate();
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"));
        String filename = "user-import-template-" + timestamp + ".xlsx";
        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=\"" + filename + "\"");
        response.getOutputStream().write(template);
        response.getOutputStream().flush();
    }

    @Operation(summary = "导出用户数据", description = "导出用户数据")
    @PostMapping("/excel/export")
    @Authorize({"allUserPermissions", "exportUser"})
    @NoRestResponse
    public void exportUsers(@RequestBody List<DataFilterDto> filters,
            @RequestParam(defaultValue = "false") boolean all,
            @RequestParam(required = false) List<String> userIds,
            HttpServletResponse response) throws IOException {
        byte[] data = userExcelService.exportUsers(filters, all, userIds);
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"));
        String filename = "users-export-" + timestamp + ".xlsx";
        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=\"" + filename + "\"");
        response.getOutputStream().write(data);
        response.getOutputStream().flush();
    }

    @Operation(summary = "导入用户数据", description = "导入用户数据")
    @PostMapping("/excel/import")
    @Authorize({"allUserPermissions", "importUser"})
    public ExcelImportResultDto importUsers(@RequestParam("file") MultipartFile file) {
        return userExcelService.importUsers(file);
    }
}
