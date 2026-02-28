package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.auth.biz.component.authserver.UserTokenBasedRememberMeServices;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.dto.auth.LoginResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.mfa.MfaValidContext;
import cn.opensrcdevelop.auth.biz.mfa.TotpAuthenticator;
import cn.opensrcdevelop.auth.biz.service.auth.WebAuthnService;
import cn.opensrcdevelop.auth.biz.service.system.password.PasswordPolicyService;
import cn.opensrcdevelop.auth.biz.service.user.LoginLogService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import java.util.ArrayList;
import java.util.List;

/**
 * 认证成功处理
 */
public class LoginSuccessHandler implements AuthenticationSuccessHandler {

    // 变更密码类型
    // 0：首次登录或密码过期
    private static final Integer CHANGE_PWD_TYPE_0 = 0;
    // 1: 密码强度不满足要求
    private static final Integer CHANGE_PWD_TYPE_1 = 1;

    private final UserService userService = SpringContextUtil.getBean(UserService.class);
    private final LoginLogService loginLogService = SpringContextUtil.getBean(LoginLogService.class);
    private final PasswordPolicyService passwordPolicyService = SpringContextUtil.getBean(PasswordPolicyService.class);
    private final UserTokenBasedRememberMeServices rememberMeServices = SpringContextUtil
            .getBean(UserTokenBasedRememberMeServices.class);
    private final WebAuthnService webAuthnService = SpringContextUtil.getBean(WebAuthnService.class);

    @SuppressWarnings("java:S3776")
    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
            Authentication authentication) {
        User user = (User) authentication.getPrincipal();
        setUserLoginInfo(user.getUserId());
        LoginResponseDto responseDto = new LoginResponseDto();
        boolean mfaValid = isValidated(request);

        // 1. 启用多因素认证
        if (Boolean.TRUE.equals(user.getEnableMfa())) {
            responseDto.setEnableMfa(true);
            List<String> mfaMethods = new ArrayList<>();

            // 1.1 设置 MFA 验证上下文
            HttpSession session = request.getSession(true);
            if (session != null) {
                MfaValidContext mfaValidContext = new MfaValidContext();
                mfaValidContext.setUserId(user.getUserId());
                mfaValidContext.setValid(mfaValid);
                mfaValidContext.setRememberMeRequested(
                        rememberMeServices.rememberMeRequested(request, AuthConstants.REMEMBER_ME));
                session.setAttribute(AuthConstants.MFA_VALID_CONTEXT, mfaValidContext);
            }

            // 1.2 检查是否有 Passkey
            boolean hasPasskey = webAuthnService.countCredentials(user.getUserId()) > 0;
            responseDto.setHasPasskey(hasPasskey);
            if (hasPasskey) {
                mfaMethods.add(AuthConstants.MFA_METHOD_WEBAUTHN);
            }

            // 1.3 检查 TOTP 设备是否绑定
            boolean totpDeviceBind = BooleanUtils.isTrue(user.getTotpDeviceBind());
            responseDto.setTotpDeviceBind(totpDeviceBind);
            if (!totpDeviceBind) {
                // 1.3.1 生成 TOTP 密钥
                String secret = TotpAuthenticator.generateSecretKey();
                User updateUser = new User();
                updateUser.setUserId(user.getUserId());
                updateUser.setTotpSecret(secret);
                userService.updateById(updateUser);

                // 1.3.2 生成二维码
                String qrCodeData = TotpAuthenticator.getQrCodeString(user.getUsername(), secret);
                responseDto.setBindTotpDeviceQrCode(CommonUtil.getBase64PngQrCode(150, 150, qrCodeData));
            } else {
                mfaMethods.add(AuthConstants.MFA_METHOD_TOTP);
            }

            // 1.4 仅一种 MFA 验证方式都没有设置时，允许添加 Passkey 凭证
            if (!hasPasskey && !totpDeviceBind) {
                responseDto
                        .setWebAuthnRegisterOptions(webAuthnService.getRegistrationOptions(user.getUserId(), request));
            }

            // 1.5 设置 MFA 认证方式
            responseDto.setMfaMethods(mfaMethods);
        } else {
            // 2. 未启用多因素认证
            responseDto.setEnableMfa(false);
        }

        // 3. 需要变更密码
        if (Boolean.TRUE.equals(user.getNeedChangePwd())) {
            responseDto.setNeedChangePwd(true);
            responseDto.setChangePwdType(CHANGE_PWD_TYPE_0);
            setChangePwdSessionFlag(request);
        }

        // 4. 控制台访问
        responseDto.setConsoleAccess(Boolean.TRUE.equals(user.getConsoleAccess()));

        // 5. 检查密码强度
        String password = request.getParameter(UsernamePasswordAuthenticationFilter.SPRING_SECURITY_FORM_PASSWORD_KEY);
        if (StringUtils.isNotEmpty(password)) {
            boolean checkRes = passwordPolicyService.checkLoginPasswordStrength(user.getUserId(),
                    request.getParameter(UsernamePasswordAuthenticationFilter.SPRING_SECURITY_FORM_PASSWORD_KEY));
            if (!checkRes) {
                responseDto.setNeedChangePwd(true);
                responseDto.setChangePwdType(CHANGE_PWD_TYPE_1);
                setChangePwdSessionFlag(request);
            }
        }

        // 6. 记住我
        if (!Boolean.TRUE.equals(responseDto.getEnableMfa()) || mfaValid) {
            rememberMeServices.loginSuccess(request, response, authentication);
        }

        WebUtil.sendJsonResponse(R.ok(responseDto), HttpStatus.OK);
    }

    /**
     * 设置用户登录信息
     *
     * @param userId
     *            用户 ID
     */
    private void setUserLoginInfo(String userId) {
        // 1. 保存登录日志
        loginLogService.saveLoginLog(userId);

        // 2. 重置最近登录失败次数
        User updateUser = new User();
        updateUser.setUserId(userId);
        updateUser.setLoginFailedCnt(0);
        userService.updateById(updateUser);
    }

    /**
     * 设置变更密码 Session 标识
     *
     * @param request
     *            请求
     */
    private void setChangePwdSessionFlag(HttpServletRequest request) {
        HttpSession session = request.getSession(true);
        if (session != null) {
            session.setAttribute(AuthConstants.SESSION_CHANGED_PWD, false);
        }
    }

    /**
     * 检查是否已通过 WebAuthn 验证
     *
     * @param request
     *            HTTP 请求
     * @return 是否已验证
     */
    private boolean isValidated(HttpServletRequest request) {
        HttpSession session = request.getSession(false);
        if (session == null) {
            return false;
        }
        // 检查统一上下文
        MfaValidContext mfaContext = (MfaValidContext) session.getAttribute(AuthConstants.MFA_VALID_CONTEXT);
        return mfaContext != null && mfaContext.isMethodValidated(AuthConstants.MFA_METHOD_WEBAUTHN);
    }
}
