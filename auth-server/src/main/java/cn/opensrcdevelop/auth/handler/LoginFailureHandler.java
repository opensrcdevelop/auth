package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.service.user.impl.UserServiceImpl;
import cn.opensrcdevelop.auth.component.AuthorizationServerProperties;
import cn.opensrcdevelop.common.response.CodeEnum;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.security.web.authentication.RememberMeServices;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import java.io.IOException;
import java.util.Objects;

/**
 * 认证失败处理
 */
@Slf4j
public class LoginFailureHandler implements AuthenticationFailureHandler {

    private final UserServiceImpl userService = (UserServiceImpl) SpringContextUtil.getBean(UserService.class);
    private final AuthorizationServerProperties authorizationServerProperties = SpringContextUtil.getBean(AuthorizationServerProperties.class);
    private final RememberMeServices rememberMeServices = SpringContextUtil.getBean(RememberMeServices.class);

    @Override
    public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response, AuthenticationException exception) throws IOException, ServletException {
        log.debug(exception.getMessage(), exception);
        this.rememberMeServices.loginFail(request, response);
        checkLoginFailedCnt(request);
        if (exception instanceof BadCredentialsException) {
            WebUtil.sendJsonResponse(R.optFail(MessageConstants.LOGIN_MSG_1002), HttpStatus.BAD_REQUEST);
            return;
        }

        if (exception instanceof DisabledException) {
            WebUtil.sendJsonResponse(R.optFail(MessageConstants.LOGIN_MSG_1003), HttpStatus.BAD_REQUEST);
            return;
        }
        WebUtil.sendJsonResponse(R.optFail(CodeEnum.RCD40001), HttpStatus.BAD_REQUEST);
    }

    /**
     * 检查用户登录失败次数
     *
     * @param request 请求
     */
    private void checkLoginFailedCnt(HttpServletRequest request) {
        // 1. 开启多次登录失败后禁用账户
        if (Boolean.TRUE.equals(authorizationServerProperties.getEnableLockAccount())) {
            String username = request.getParameter(UsernamePasswordAuthenticationFilter.SPRING_SECURITY_FORM_USERNAME_KEY);

            // 1.1 检索用户
            User loadedUser = (User) userService.loadUserByUsername(username);
            if (Objects.nonNull(loadedUser)) {
                User updateUser = new User();
                updateUser.setUserId(loadedUser.getUserId());

                int failedCnt = loadedUser.getLoginFailedCnt() != null ? loadedUser.getLoginFailedCnt() : 0;
                // 1.2 递增登录失败次数
                failedCnt++;
                updateUser.setLoginFailedCnt(failedCnt);

                // 1.3 判断是否超过允许的最大登录失败次数
                Integer maxFailedCnt = authorizationServerProperties.getMaxLoginFailedCnt();
                if (Objects.nonNull(maxFailedCnt) && maxFailedCnt > 0 && failedCnt > maxFailedCnt) {
                    // 1.3.1 禁用账户
                    updateUser.setLocked(Boolean.TRUE);
                }
                userService.updateById(updateUser);
            }
        }
    }
}
