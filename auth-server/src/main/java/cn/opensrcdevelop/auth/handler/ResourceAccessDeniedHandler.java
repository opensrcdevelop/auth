package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.common.response.CodeEnum;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.web.access.AccessDeniedHandler;

/**
 * 资源服务器鉴权失败处理
 */
@Slf4j
public class ResourceAccessDeniedHandler implements AccessDeniedHandler {

    @Override
    public void handle(HttpServletRequest request, HttpServletResponse response,
            AccessDeniedException accessDeniedException) throws IOException, ServletException {
        log.debug(accessDeniedException.getMessage(), accessDeniedException);
        WebUtil.sendJsonResponse(R.optFail(CodeEnum.RCD40003), HttpStatus.FORBIDDEN);
    }
}
