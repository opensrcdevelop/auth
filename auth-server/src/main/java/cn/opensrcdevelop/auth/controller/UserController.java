package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.annocation.ResourceLimit;
import cn.opensrcdevelop.auth.biz.dto.*;
import cn.opensrcdevelop.auth.biz.service.UserAttrService;
import cn.opensrcdevelop.auth.biz.service.UserService;
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
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

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

    @Operation(summary = "创建用户", description = "创建用户")
    @PostMapping
    @Authorize({ "allUserPermissions", "createUser" })
    public void createUser(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) UserRequestDto requestDto) {
        userService.createUser(requestDto);
    }

    @Operation(summary = "创建用户属性", description = "创建用户属性")
    @PostMapping("/attr")
    @Authorize({ "allUserAttrPermissions", "createUserAttr" })
    public void createUserAttr(@RequestBody @Validated({ ValidationGroups.Operation.INSERT.class }) UserAttrRequestDto requestDto) {
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
    @Authorize({ "allUserAttrPermissions", "listUserAttr" })
    public PageData<UserAttrResponseDto> listUserAttrs(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size,
                                                   @RequestParam(required = false, defaultValue = "false") Boolean onlyDisplay, @RequestParam(required = false) String keyword) {
        return userAttrService.listUserAttrs(page, size, onlyDisplay, keyword);
    }

    @Operation(summary = "获取用户列表", description = "获取用户列表")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true)
    })
    @PostMapping("/list")
    @Authorize({ "allUserPermissions", "listUser" })
    public PageData<Map<String, Object>> list(@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size, @RequestBody @Valid List<DataFilterRequestDto> filters) {
        return userService.list(page, size, filters);
    }

    @Operation(summary = "更新用户信息", description = "更新用户信息")
    @PutMapping
    @Authorize({ "allUserPermissions", "updateUser" })
    @ResourceLimit(ids = { "4a7eb192-b0e8-4678-bf81-bbbd70ba1880", "d0d5d9fc-30cd-456b-9f20-ca0559cb7131" }, idEl = "#requestDto.userId")
    public void updateUser(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) UserRequestDto requestDto) {
        userService.updateUser(requestDto);
    }

    @Operation(summary = "更新用户属性", description = "更新用户属性")
    @PutMapping("/attr")
    @Authorize({ "allUserAttrPermissions", "updateUserAttr" })
    @ResourceLimit(ids = {
            "47c3b7fb-fbce-4410-aa1e-3b1353468d49",
            "08604464-2832-44e3-8bd4-d8dbda7db9d7",
            "ff289375-461b-4e6f-8e16-9187d7e44a14",
            "0965fca9-d005-4cd8-8a77-531d01b8fc05",
            "6a5a3759-3fb3-47c3-bec6-f14d32e170c2",
            "3845b5d4-36a0-45bb-854e-6d79593aefd4",
            "d019fb4e-8acd-4411-9061-9d8aee961703",
            "3cb048e2-5896-46aa-96e6-fa975b4780f5"
    }, idEl = "#requestDto.id")
    public void updateUserAttr(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) UserAttrRequestDto requestDto) {
        userAttrService.updateUserAttr(requestDto);
    }

    @Operation(summary = "设置用户属性显示顺序", description = "设置用户属性显示顺序")
    @PostMapping("/attr/seq")
    @Authorize({ "allUserAttrPermissions", "setUserAttrDisplaySeq" })
    public void setUserAttrDisplaySeq(@RequestBody @NotEmpty @Valid List<SetUserAttrDisplaySeqRequestDto> requestDtoList) {
        userAttrService.setUserAttrDisplaySeq(requestDtoList);
    }

    @Operation(summary = "获取用户详情", description = "获取用户详情")
    @Parameters({
            @Parameter(name = "id", description = "用户ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/{id}")
    @Authorize({ "allUserPermissions", "getUserDetail" })
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
    @Authorize({ "allUserPermissions", "deleteUser" })
    @ResourceLimit(ids = { "4a7eb192-b0e8-4678-bf81-bbbd70ba1880", "d0d5d9fc-30cd-456b-9f20-ca0559cb7131" }, idEl = "#id")
    public void removeUser(@PathVariable @NotBlank String id) {
        userService.removeUser(id);
    }

    @Operation(summary = "获取用户属性详情", description = "获取用户属性详情")
    @Parameters({
            @Parameter(name = "id", description = "用户属性ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/attr/{id}")
    @Authorize({ "allUserAttrPermissions", "getUserAttrDetail" })
    public UserAttrResponseDto userAttrDetail(@PathVariable @NotBlank String id) {
        return userAttrService.detail(id);
    }

    @Operation(summary = "删除用户属性", description = "删除用户属性")
    @DeleteMapping("/attr/{id}")
    @Authorize({ "allUserAttrPermissions", "deleteUserAttr" })
    @ResourceLimit(ids = {
            "47c3b7fb-fbce-4410-aa1e-3b1353468d49",
            "08604464-2832-44e3-8bd4-d8dbda7db9d7",
            "ff289375-461b-4e6f-8e16-9187d7e44a14",
            "0965fca9-d005-4cd8-8a77-531d01b8fc05",
            "6a5a3759-3fb3-47c3-bec6-f14d32e170c2",
            "3845b5d4-36a0-45bb-854e-6d79593aefd4",
            "d019fb4e-8acd-4411-9061-9d8aee961703",
            "3cb048e2-5896-46aa-96e6-fa975b4780f5",
            "37479a6e-e777-4114-9f81-c8e31f0ce49b",
            "1c092d28-81fb-4e97-92ef-93d446c826c6",
            "edf63d7e-fa66-4483-b38b-1cd7200b05ec"
    }, idEl = "#id")
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
    @Authorize({ "allUserPermissions", "rebindMfaDevice" })
    @ResourceLimit(ids = { "4a7eb192-b0e8-4678-bf81-bbbd70ba1880", "d0d5d9fc-30cd-456b-9f20-ca0559cb7131" }, idEl = "#id")
    public void rebindMfaDevice(@PathVariable @NotBlank String id) {
        userService.rebindMfaDevice(id);
    }

    @Operation(summary = "清空授权的 Token", description = "清空授权的 Token")
    @Parameters({
            @Parameter(name = "id", description = "用户ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/{id}/token")
    @Authorize({ "allUserPermissions", "clearTokens" })
    @ResourceLimit(ids = { "4a7eb192-b0e8-4678-bf81-bbbd70ba1880", "d0d5d9fc-30cd-456b-9f20-ca0559cb7131" }, idEl = "#id")
    public void clearAuthorizedToken(@PathVariable @NotBlank String id) {
        userService.clearAuthorizedTokens(id);
    }

    @Operation(summary = "重置密码", description = "重置密码")
    @PostMapping("/me/password/reset")
    @ResourceLimit(ids = { "4a7eb192-b0e8-4678-bf81-bbbd70ba1880" }, idEl = "#id")
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
}
