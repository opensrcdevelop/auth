package cn.opensrcdevelop.common.util;

import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import eu.bitwalker.useragentutils.UserAgent;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Optional;

@Slf4j
@SuppressWarnings("unused")
public class WebUtil {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    static {
        OBJECT_MAPPER.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        OBJECT_MAPPER.registerModule(new JavaTimeModule());
        OBJECT_MAPPER.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    }

    private WebUtil () {}

    public static void sendJsonResponse(Object object, HttpStatus status) {
        getResponse().ifPresent(response -> {
            try {
                response.setContentType(MediaType.APPLICATION_JSON_VALUE);
                response.setCharacterEncoding(StandardCharsets.UTF_8.displayName());
                response.setStatus(status.value());
                response.getWriter().write(OBJECT_MAPPER.writeValueAsString(object));
                response.getWriter().flush();
            } catch (IOException e) {
                throw new ServerException(e);
            }
        });
    }

    public static Optional<HttpServletRequest> getRequest() {
        ServletRequestAttributes requestAttributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (requestAttributes != null) {
            return Optional.of(requestAttributes.getRequest());
        }
        return Optional.empty();
    }

    public static Optional<HttpServletResponse> getResponse() {
        ServletRequestAttributes requestAttributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (requestAttributes != null) {
            return Optional.ofNullable(requestAttributes.getResponse());
        }
        return Optional.empty();
    }

    public static String getRemoteIP(HttpServletRequest request) {
        String ipAddress = request.getHeader("x-forwarded-for");

        if (ipAddress == null || ipAddress.isEmpty() || CommonConstants.UNKNOWN.equalsIgnoreCase(ipAddress)) {
            ipAddress = request.getHeader("Proxy-Client-IP");
        }

        if (ipAddress == null || ipAddress.isEmpty() || CommonConstants.UNKNOWN.equalsIgnoreCase(ipAddress)) {
            ipAddress = request.getHeader("WL-Proxy-Client-IP");
        }

        if (ipAddress == null || ipAddress.isEmpty() || CommonConstants.UNKNOWN.equalsIgnoreCase(ipAddress)) {
            ipAddress = request.getRemoteAddr();
            if ("127.0.0.1".equals(ipAddress) || "0:0:0:0:0:0:0:1".equals(ipAddress)) {
                InetAddress inet = null;
                try {
                    inet = InetAddress.getLocalHost();
                    ipAddress= inet.getHostAddress();
                } catch (UnknownHostException e) {
                    log.error(e.getMessage(), e);
                }
            }
        }

        // 对于通过多个代理的情况，第一个IP为客户端真实IP,多个IP按照','分割
        // "***.***.***.***".length() = 15
        if (ipAddress != null && ipAddress.length() > 15 && ipAddress.contains(",")) {
            ipAddress = ipAddress.substring(0, ipAddress.indexOf(","));

        }
        return ipAddress;
    }

    @SuppressWarnings("all")
    public static String getRemoteIP() {
        ServletRequestAttributes servletRequestAttributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (servletRequestAttributes != null) {
            var request =  servletRequestAttributes.getRequest();
            return getRemoteIP(request);
        }
        return null;
    }

    /**
     * 获取请求参数
     *
     * @param request 请求
     * @return 请求参数
     */
    public static MultiValueMap<String, String> getParameters(HttpServletRequest request) {
        Map<String, String[]> parameterMap = request.getParameterMap();
        MultiValueMap<String, String> parameters = new LinkedMultiValueMap<>(parameterMap.size());
        parameterMap.forEach((k, v) -> {
            for(String value : v) {
                parameters.add(k, value);
            }
        });
        return parameters;
    }

    /**
     * 获取请求设备类型
     *
     * @return 设备类型
     */
    public static String getDeviceType() {
        UserAgent userAgent = getUserAgent();
        if (userAgent != null) {
            return userAgent.getOperatingSystem().getDeviceType().getName();
        }
        return CommonConstants.UNKNOWN;
    }

    /**
     * 获取请求设备操作系统
     *
     * @return 设备操作系统
     */
    public static String getDeviceOs() {
        UserAgent userAgent = getUserAgent();
        if (userAgent != null) {
            return userAgent.getOperatingSystem().getName();
        }
        return CommonConstants.UNKNOWN;
    }

    /**
     * 获取请求浏览器类型
     *
     * @return 浏览器类型
     */
    public static String getBrowserType() {
        UserAgent userAgent = getUserAgent();
        if (userAgent != null) {
            return userAgent.getBrowser().getName();
        }
        return CommonConstants.UNKNOWN;
    }

    private static UserAgent getUserAgent() {
        Optional<HttpServletRequest> request = getRequest();
        return request.map(httpServletRequest -> UserAgent.parseUserAgentString(httpServletRequest.getHeader("User-Agent"))).orElse(null);
    }
}
