package cn.opensrcdevelop.auth.filter;

import cn.opensrcdevelop.common.config.AuthorizationServerProperties;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.util.Objects;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.web.authentication.RememberMeServices;
import org.springframework.security.web.authentication.rememberme.RememberMeAuthenticationFilter;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.filter.OncePerRequestFilter;

public class OAuth2AuthorizeRememberMeAuthenticationFilter extends OncePerRequestFilter {

    private static final AntPathMatcher MATCHER = new AntPathMatcher();

    private final RememberMeAuthenticationFilter delegateFilter;

    public OAuth2AuthorizeRememberMeAuthenticationFilter(AuthenticationManager authenticationManager,
            RememberMeServices rememberMeServices) {
        this.delegateFilter = new RememberMeAuthenticationFilter(authenticationManager, rememberMeServices);
    }

    @Override
    @SuppressWarnings("NullableProblems")
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        String apiPrefix = SpringContextUtil.getBean(AuthorizationServerProperties.class).getApiPrefix();
        if (MATCHER.match(apiPrefix.concat("/oauth2/authorize"), request.getServletPath())) {
            delegateFilter.doFilter(request, response, filterChain);
            HttpSession session = request.getSession(false);
            if (Objects.nonNull(session)) {
                session.invalidate();
            }
            return;
        }
        filterChain.doFilter(request, response);
    }
}
