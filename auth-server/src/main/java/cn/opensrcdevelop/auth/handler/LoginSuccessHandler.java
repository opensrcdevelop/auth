package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.dto.LoginResponseDto;
import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.mfa.MultiFactorAuthenticator;
import cn.opensrcdevelop.auth.biz.mfa.TotpValidContext;
import cn.opensrcdevelop.auth.biz.service.LoginLogService;
import cn.opensrcdevelop.auth.biz.service.UserService;
import cn.opensrcdevelop.auth.component.AuthorizationServerProperties;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;

import java.io.IOException;

/**
 * 认证成功处理
 */
public class LoginSuccessHandler implements AuthenticationSuccessHandler {

    private final UserService userService = SpringContextUtil.getBean(UserService.class);
    private final LoginLogService loginLogService = SpringContextUtil.getBean(LoginLogService.class);
    private final AuthorizationServerProperties authorizationServerProperties = SpringContextUtil.getBean(AuthorizationServerProperties.class);

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException, ServletException {
        User user = (User) authentication.getPrincipal();
        setUserLoginInfo(user.getUserId());
        LoginResponseDto responseDto = new LoginResponseDto();

        // 1. 启用多因素认证
        if (Boolean.TRUE.equals(user.getEnableMfa())) {
            // 1.1 未绑定设备
            if (BooleanUtils.isNotTrue(user.getMfaDeviceBind())) {
                // 1.1.1 生成 TOTP 密钥（不存在的场合下）
                String secret = user.getMfaSecret();
                if (StringUtils.isEmpty(secret)) {
                    secret = MultiFactorAuthenticator.generateSecretKey();

                    // 1.1.2 更新用户数据
                    User updateUser = new User();
                    updateUser.setUserId(user.getUserId());
                    updateUser.setMfaSecret(secret);
                    userService.updateById(updateUser);
                }

                // 1.1.3 生成二维码数据
                String qrCodeData = MultiFactorAuthenticator.getQrCodeString(user.getUsername(), secret);

                responseDto.setEnableMfa(true);
                responseDto.setBound(false);
                responseDto.setQrCode(CommonUtil.getBase64PngQrCode(150, 150, qrCodeData));
            } else {
                // 1.2 已绑定设备
                responseDto.setEnableMfa(true);
                responseDto.setBound(true);
            }

            // 1.3 设置动态密码校验结果
            HttpSession session = request.getSession(true);
            if (session != null) {
                TotpValidContext totpValidContext = new TotpValidContext();
                totpValidContext.setValid(false);
                totpValidContext.setUserId(user.getUserId());
                session.setAttribute(AuthConstants.TOTP_VALID_CONTEXT, totpValidContext);
            }
        } else {
            // 2. 未启用多因素认证
            responseDto.setEnableMfa(false);
        }

        // 3. 需要变更密码
        if (Boolean.TRUE.equals(user.getNeedChangePwd())) {
            HttpSession session = request.getSession(true);
            if (session != null) {
                // 3.1 设置变更密码结果
                session.setAttribute(AuthConstants.SESSION_CHANGED_PWD, false);
                responseDto.setNeedChangePwd(true);
            }
        }

        // 4. 控制台访问
        responseDto.setConsoleAccess(Boolean.TRUE.equals(user.getConsoleAccess()));

        WebUtil.sendJsonResponse(R.ok(responseDto), HttpStatus.OK);
    }

    /**
     * 设置用户登录信息
     *
     * @param userId 用户 ID
     */
    private void setUserLoginInfo(String userId) {
        // 1. 保存登录日志
        loginLogService.saveLoginLog(userId, authorizationServerProperties.getMaxLoginLogNum());

        // 2. 重置最近登录失败次数
        User updateUser = new User();
        updateUser.setUserId(userId);
        updateUser.setLoginFailedCnt(0);
        userService.updateById(updateUser);
    }
}
