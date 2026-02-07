package cn.opensrcdevelop.auth.handler;

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
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.RememberMeServices;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

/**
 * 认证成功处理
 */
public class LoginSuccessHandler implements AuthenticationSuccessHandler {

    // 变更密码类型
    // 0：首次登录或密码过期
    private static final Integer CHANGE_PWD_TYPE_0 = 0;
    // 1: 密码强度不满足要求
    private static final Integer CHANGE_PWD_TYPE_1 = 1;

    private static final String MFA_METHOD_TOTP = "TOTP";
    private static final String MFA_METHOD_WEBAUTHN = "WEBAUTHN";

    private final UserService userService = SpringContextUtil.getBean(UserService.class);
    private final LoginLogService loginLogService = SpringContextUtil.getBean(LoginLogService.class);
    private final PasswordPolicyService passwordPolicyService = SpringContextUtil.getBean(PasswordPolicyService.class);
    private final RememberMeServices rememberMeServices = SpringContextUtil.getBean(RememberMeServices.class);
    private final WebAuthnService webAuthnService = SpringContextUtil.getBean(WebAuthnService.class);

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
            Authentication authentication) throws IOException, ServletException {
        User user = (User) authentication.getPrincipal();
        setUserLoginInfo(user.getUserId());
        LoginResponseDto responseDto = new LoginResponseDto();

        // 1. 启用多因素认证
        if (Boolean.TRUE.equals(user.getEnableMfa())) {
            responseDto.setEnableMfa(true);

            // 收集支持的 MFA 方式
            List<String> supportedMethods = new ArrayList<>();
            Set<String> requiredMethods = new HashSet<>();

            // 1.1 检查 WebAuthn
            boolean hasWebAuthn = webAuthnService.countCredentials(user.getUserId()) > 0;
            if (hasWebAuthn) {
                supportedMethods.add(MFA_METHOD_WEBAUTHN);
                // 如果是 Passkey 登录，WebAuthn 已经验证
                if (webAuthnService.isValidated(request)) {
                    requiredMethods.add(MFA_METHOD_WEBAUTHN);
                }
            }

            // 1.2 检查 TOTP
            boolean hasTotp = StringUtils.isNotBlank(user.getTotpSecret())
                    && BooleanUtils.isTrue(user.getTotpDeviceBind());
            if (hasTotp) {
                supportedMethods.add(MFA_METHOD_TOTP);
                // TOTP 总是需要验证
                requiredMethods.add(MFA_METHOD_TOTP);
            }

            // 设置支持的 MFA 方式
            responseDto.setSupportedMfaMethods(supportedMethods);

            // 如果没有任何支持的 MFA 方式，禁用 MFA
            if (supportedMethods.isEmpty()) {
                responseDto.setEnableMfa(false);
            } else {
                // 1.3 设置 MFA 验证上下文
                HttpSession session = request.getSession(true);
                if (session != null) {
                    MfaValidContext mfaValidContext = new MfaValidContext();
                    mfaValidContext.setUserId(user.getUserId());

                    // 如果 WebAuthn 已验证，添加到已验证列表
                    if (webAuthnService.isValidated(request)) {
                        mfaValidContext.addValidatedMethod(MFA_METHOD_WEBAUTHN);
                        mfaValidContext.setValid(true);
                    } else {
                        mfaValidContext.setValid(false);
                    }

                    session.setAttribute(AuthConstants.MFA_VALID_CONTEXT, mfaValidContext);
                }

                // 1.4 TOTP 未绑定的处理
                if (hasTotp && BooleanUtils.isNotTrue(user.getTotpDeviceBind())) {
                    // 生成 TOTP 密钥
                    String secret = TotpAuthenticator.generateSecretKey();
                    User updateUser = new User();
                    updateUser.setUserId(user.getUserId());
                    updateUser.setTotpSecret(secret);
                    userService.updateById(updateUser);

                    // 生成二维码
                    String qrCodeData = TotpAuthenticator.getQrCodeString(user.getUsername(), secret);
                    responseDto.setBound(false);
                    responseDto.setQrCode(CommonUtil.getBase64PngQrCode(150, 150, qrCodeData));
                } else {
                    responseDto.setBound(true);
                }
            }
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
        boolean checkRes = passwordPolicyService.checkLoginPasswordStrength(user.getUserId(),
                request.getParameter(UsernamePasswordAuthenticationFilter.SPRING_SECURITY_FORM_PASSWORD_KEY));
        if (!checkRes) {
            responseDto.setNeedChangePwd(true);
            responseDto.setChangePwdType(CHANGE_PWD_TYPE_1);
            setChangePwdSessionFlag(request);
        }

        // 6. 记住我
        rememberMeServices.loginSuccess(request, response, authentication);

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
}
