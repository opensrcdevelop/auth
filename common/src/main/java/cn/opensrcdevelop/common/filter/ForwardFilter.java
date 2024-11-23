package cn.opensrcdevelop.common.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;

import java.io.IOException;

@RequiredArgsConstructor
public class ForwardFilter extends RestFilter {

    private final String targetUrl;

    @Override
    protected void doSubFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        // 转发至目标路径
        request.getRequestDispatcher(targetUrl).forward(request, response);
    }
}
