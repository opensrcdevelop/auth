package cn.opensrcdevelop.common.filter;

import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.WebUtil;
import com.alibaba.ttl.TransmittableThreadLocal;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.util.StopWatch;

@Slf4j
public class TraceFilter extends RestFilter {

    public static final TransmittableThreadLocal<Map<String, String>> TTL_MDC = new TransmittableThreadLocal<>() {

        /**
         * 在多线程数据传递的时候，将数据复制给子线程 MDC
         */
        @Override
        protected void beforeExecute() {
            final Map<String, String> map = get();
            map.forEach(MDC::put);
        }

        @Override
        protected void afterExecute() {
            MDC.clear();
        }

        @Override
        protected Map<String, String> initialValue() {
            return new HashMap<>();
        }
    };

    @Override
    protected void doSubFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {
        try {
            String traceId = UUID.randomUUID().toString();
            String remoteIP = WebUtil.getRemoteIP(request);
            response.addHeader(CommonConstants.REP_HEADER_X_REQUEST_ID, traceId);
            MDC.put(CommonConstants.MDC_TRACE_ID, traceId);
            MDC.put(CommonConstants.MDC_REMOTE_IP, remoteIP);
            TTL_MDC.get().put(CommonConstants.MDC_TRACE_ID, traceId);
            TTL_MDC.get().put(CommonConstants.MDC_REMOTE_IP, remoteIP);
            doNext(filterChain, request, response, traceId);
        } finally {
            MDC.clear();
            TTL_MDC.get().clear();
            TTL_MDC.remove();

        }
    }

    private void doNext(FilterChain filterChain, HttpServletRequest request, HttpServletResponse response,
            String requestId) throws ServletException, IOException {
        // API 审计
        StopWatch stopWatch = new StopWatch(requestId);
        log.info("REQUEST API: {} {}", request.getMethod(), request.getRequestURI());
        stopWatch.start();
        filterChain.doFilter(request, response);
        stopWatch.stop();
        log.info("API: {} {} | status: {} | time: {}ms", request.getMethod(), request.getRequestURI(),
                response.getStatus(), stopWatch.getTotalTime(TimeUnit.MILLISECONDS));
    }
}
