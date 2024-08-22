package cn.opensrcdevelop.auth.client.support;

import cn.opensrcdevelop.common.util.WebUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.Assert;

@Slf4j
public class OAuth2ContextHolder {

    private OAuth2ContextHolder() {}

    private static final String SESSION_OAUTH2_CONTEXT = "OAUTH2_CONTEXT";

    public static OAuth2Context getContext() {
        var webRequest = WebUtil.getRequest();
        if (webRequest.isPresent()) {
            var session = webRequest.get().getSession();
            if (session != null) {
                log.debug("从 session: {} 中获取 OAuthContext", session.getId());
                return (OAuth2Context) session.getAttribute(SESSION_OAUTH2_CONTEXT);
            }
        }
        return null;
    }

    public static void clearContext() {
        WebUtil.getRequest().ifPresent(request -> {
            var session = request.getSession();
            if (session != null) {
                session.removeAttribute(SESSION_OAUTH2_CONTEXT);
            }
        });
    }

    @SuppressWarnings("all")
    public static void setContext(OAuth2Context context) {
        Assert.notNull(context, "OAuth2Context can not be null");
        WebUtil.getRequest().ifPresent(request -> {
            var session = request.getSession(false);
            if (session != null) {
                session.setAttribute(SESSION_OAUTH2_CONTEXT, context);
                log.debug("设置 OAuth2Context 到 session: {} 中", session.getId());
            }
        });
    }
}
