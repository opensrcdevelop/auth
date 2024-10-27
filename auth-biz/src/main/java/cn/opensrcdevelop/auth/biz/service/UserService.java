package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.*;
import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletRequest;

import java.util.List;
import java.util.Map;

public interface UserService extends IService<User> {

    void createUser(UserRequestDto requestDto);

    User getUserInfo(String userId, String resourceGroupCode);

    PageData<Map<String, Object>> list(int page, int size, List<DataFilterRequestDto> filters);

    void updateUser(UserRequestDto requestDto);

    UserResponseDto detail(String userId);

    void changePwd(ChangePwdRequestDto requestDto, HttpServletRequest request);

    void removeUser(String userId);

    Map<String, Object> getCurrentUserInfo();

    void rebindMfaDevice(String userId);

    void clearAuthorizedTokens(String userId);

    void resetPwd(ResetPwdRequestDto requestDto);

    void updateMe(Map<String, Object> userInfo);

    void bindEmail(BindOrUnbindEmailRequestDto requestDto);

    void unbindEmail(BindOrUnbindEmailRequestDto requestDto);

    PageData<PermissionResponseDto> getPermissions(int page, int size, String userId, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);
}
