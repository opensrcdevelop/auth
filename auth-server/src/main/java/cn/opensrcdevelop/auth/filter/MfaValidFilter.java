package cn.opensrcdevelop.auth.filter;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.mfa.MfaValidContext;
import cn.opensrcdevelop.common.filter.RestFilter;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.http.HttpStatus;

/**
 * MFA 验证过滤器（统一处理 TOTP 和 WebAuthn 验证）
 */
public class MfaValidFilter extends RestFilter {

    /**
     * 需要排除 MFA 验证的路径
     */
    private static final String[] EXCLUDE_PATHS = {
            "/webauthn/",
            "/login/passkey"
    };

    @Override
    protected void doSubFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {
        // 排除 WebAuthn API 和 Passkey 登录路径
        String requestUri = request.getRequestURI();
        for (String excludePath : EXCLUDE_PATHS) {
            if (requestUri.contains(excludePath)) {
                filterChain.doFilter(request, response);
                return;
            }
        }

        HttpSession session = request.getSession(false);
        if (session != null) {
            MfaValidContext mfaValidContext = (MfaValidContext) session
                    .getAttribute(AuthConstants.MFA_VALID_CONTEXT);
            if (mfaValidContext != null && BooleanUtils.isFalse(mfaValidContext.getValid())) {
                // MFA 未验证通过，返回错误
                WebUtil.sendJsonResponse(R.optFail(MessageConstants.TOTP_MSG_1001, new Object()),
                        HttpStatus.UNAUTHORIZED);
                return;
            }
        }
        filterChain.doFilter(request, response);
    }
}
