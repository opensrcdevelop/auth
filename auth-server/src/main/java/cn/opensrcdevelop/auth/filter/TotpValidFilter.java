package cn.opensrcdevelop.auth.filter;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.mfa.TotpValidContext;
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

public class TotpValidFilter extends RestFilter {

    @Override
    protected void doSubFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {
        HttpSession session = request.getSession(false);
        if (session != null) {
            TotpValidContext totpValidContext = (TotpValidContext) session
                    .getAttribute(AuthConstants.TOTP_VALID_CONTEXT);
            if (totpValidContext != null && BooleanUtils.isFalse(totpValidContext.getValid())) {
                WebUtil.sendJsonResponse(R.optFail(MessageConstants.TOTP_MSG_1001, new Object()),
                        HttpStatus.UNAUTHORIZED);
                return;
            }
        }
        filterChain.doFilter(request, response);
    }
}
