package cn.opensrcdevelop.common.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.filter.OncePerRequestFilter;

public abstract class RestFilter extends OncePerRequestFilter {

    private static final AntPathMatcher MATCHER = new AntPathMatcher();
    private final List<String> excludePatterns = new ArrayList<>();

    @Override
    @SuppressWarnings("NullableProblems")
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        String path = request.getServletPath();
        // 排除非过滤目标的路径
        if (excludePatterns.stream().anyMatch(p -> MATCHER.match(p, path))) {
            filterChain.doFilter(request, response);
            return;
        }
        doSubFilterInternal(request, response, filterChain);
    }

    public void excludePathPatterns(String... patterns) {
        excludePatterns.addAll(Arrays.asList(patterns));
    }

    protected abstract void doSubFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException;
}
