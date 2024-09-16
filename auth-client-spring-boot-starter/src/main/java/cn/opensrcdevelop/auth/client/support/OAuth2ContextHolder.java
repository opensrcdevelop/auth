package cn.opensrcdevelop.auth.client.support;

import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.Assert;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.Optional;

@Slf4j
public class OAuth2ContextHolder {

    private OAuth2ContextHolder() {}

    private static final String SESSION_OAUTH2_CONTEXT = "OAUTH2_CONTEXT";

    public static OAuth2Context getContext() {
        var webRequest = getRequest();
        if (webRequest.isPresent()) {
            var session = webRequest.get().getSession(false);
            if (session != null) {
                return (OAuth2Context) session.getAttribute(SESSION_OAUTH2_CONTEXT);
            }
        }
        return null;
    }

    public static void clearContext() {
        getRequest().ifPresent(request -> {
            var session = request.getSession(false);
            if (session != null) {
                session.removeAttribute(SESSION_OAUTH2_CONTEXT);
            }
        });
    }

    @SuppressWarnings("all")
    public static void setContext(OAuth2Context context) {
        Assert.notNull(context, "OAuth2Context can not be null");
        getRequest().ifPresent(request -> {
            var session = request.getSession(true);
            if (session != null) {
                session.setAttribute(SESSION_OAUTH2_CONTEXT, context);
                log.debug("设置 OAuth2Context 到 session: {} 中", session.getId());
            }
        });
    }

    private static Optional<HttpServletRequest> getRequest() {
        ServletRequestAttributes requestAttributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (requestAttributes != null) {
            return Optional.of(requestAttributes.getRequest());
        }
        return Optional.empty();
    }
}
