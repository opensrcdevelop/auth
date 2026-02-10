package cn.opensrcdevelop.auth.filter;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.common.filter.RestFilter;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import org.springframework.http.HttpStatus;

import java.io.IOException;

public class ChangePwdCheckFilter extends RestFilter {

    @Override
    protected void doSubFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {
        HttpSession session = request.getSession(false);
        if (session != null) {
            Boolean changeResult = (Boolean) session.getAttribute(AuthConstants.SESSION_CHANGED_PWD);
            if (Boolean.FALSE.equals(changeResult)) {
                WebUtil.sendJsonResponse(R.optFail(MessageConstants.LOGIN_MSG_1000, new Object()),
                        HttpStatus.UNAUTHORIZED);
                return;
            }
        }
        filterChain.doFilter(request, response);
    }
}
