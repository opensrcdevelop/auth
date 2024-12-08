package cn.opensrcdevelop.common.util;

import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import com.blueconic.browscap.Capabilities;
import com.blueconic.browscap.UserAgentParser;
import com.blueconic.browscap.UserAgentService;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import io.vavr.control.Try;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.lionsoul.ip2region.xdb.Searcher;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.session.SessionRepository;
import org.springframework.session.web.http.SessionRepositoryFilter;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.URI;
import java.net.URL;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j
@SuppressWarnings("unused")
public class WebUtil {

    private static final String REQ_HEADER_USER_AGENT = "User-Agent";
    private static final String IP_REGION_COUNTRY_CHAIN = "中国";
    private static final String IP_REGION_ISP_INTERNAL = "内网IP";

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
    private static final UserAgentParser USER_AGENT_PARSER = Try
            .of(() -> new UserAgentService().loadParser())
            .getOrElseThrow(e -> {
                log.error("Failed to init UserAgentParser", e);
                return new IllegalStateException("Failed to init UserAgentParser");
            });
    private static final Searcher IP_REGION_SEARCHER = Try
            .of(() -> {
                try (InputStream inputStream = new ClassPathResource("/ipdb/ip2region.xdb").getInputStream()) {
                    return Searcher.newWithBuffer(inputStream.readAllBytes());
                }
            }).getOrElseThrow(e -> {
                log.error("Failed to load ip region db into memory");
                return new IllegalStateException("Failed to load ip region db into memory");
            });

    static {
        OBJECT_MAPPER.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        OBJECT_MAPPER.registerModule(new JavaTimeModule());
        OBJECT_MAPPER.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    }

    private WebUtil () {}

    public static void sendJsonResponse(Object object, HttpStatus status) {
        getResponse().ifPresent(response -> sendJsonResponse(response, object, status));
    }

    public static void sendJsonResponse(HttpServletResponse response, Object object, HttpStatus status) {
        try {
            response.setContentType(MediaType.APPLICATION_JSON_VALUE);
            response.setCharacterEncoding(StandardCharsets.UTF_8.displayName());
            response.setStatus(status.value());
            response.getWriter().write(OBJECT_MAPPER.writeValueAsString(object));
            response.getWriter().flush();
        } catch (IOException e) {
            throw new ServerException(e);
        }
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
        Capabilities userAgent = getUserAgent();
        if (userAgent != null) {
            return userAgent.getDeviceType();
        }
        return CommonConstants.UNKNOWN;
    }

    /**
     * 获取请求设备操作系统
     *
     * @return 设备操作系统
     */
    public static String getDeviceOs() {
        Capabilities userAgent = getUserAgent();
        if (userAgent != null) {
            return userAgent.getPlatform() + "/" + userAgent.getPlatformVersion();
        }
        return CommonConstants.UNKNOWN;
    }

    /**
     * 获取请求浏览器类型
     *
     * @return 浏览器类型
     */
    public static String getBrowserType() {
        Capabilities userAgent = getUserAgent();
        if (userAgent != null) {
            return userAgent.getBrowser() + "/" + userAgent.getBrowserMajorVersion();
        }
        return CommonConstants.UNKNOWN;
    }

    /**
     * 获取当前请求的根 URL
     *
     * @return 当前请求的根 URL
     */
    public static String getRootUrl() {
        if (getRequest().isPresent()) {
            return Try.of(() -> {
                URL url = new URI(getRequest().get().getRequestURL().toString()).toURL();
                return String.format(CommonConstants.URL_FORMAT, url.getProtocol(), url.getAuthority());
            }).getOrElseThrow(ServerException::new);
        }
        return StringUtils.EMPTY;
    }

    /**
     * 获取 IP 属地
     *
     * @param ipAddress IP 地址
     * @return IP 属地
     */
    public static String getIpRegion(String ipAddress) {
        try {
            // 数据格式： 国家|区域|省份|城市|ISP
            String searchRes = IP_REGION_SEARCHER.search(ipAddress);
            if (StringUtils.isNotEmpty(searchRes)) {
                String[] searchResParts = searchRes.split("\\|");
                if (searchResParts.length > 0) {
                    if (IP_REGION_COUNTRY_CHAIN.equals(searchResParts[0])) {
                        // 国内属地：省份-城市-提供商
                        searchResParts[0] = "0";
                        return concatIpRegion(searchResParts);
                    } else if ("0".equals(searchResParts[0])) {
                        // 内网IP
                        if (IP_REGION_ISP_INTERNAL.equals(searchResParts[4])) {
                            return searchResParts[4];
                        } else {
                            return CommonConstants.UNKNOWN;
                        }
                    } else {
                        // 国外属地：国家
                        return searchResParts[0];
                    }
                }
            }
            return CommonConstants.UNKNOWN;
        } catch (Exception e) {
            log.warn("无法获取 IP 地址：{} 的属地", ipAddress);
            return CommonConstants.UNKNOWN;
        }
    }

    /**
     * 删除 session
     *
     * @param sessionId session ID
     */
    public static void removeSession(String sessionId) {
        getRequest().ifPresent(request -> {
            HttpSession session = request.getSession();
            if (Objects.nonNull(session)) {
                if (StringUtils.equals(sessionId, session.getId())) {
                    session.invalidate();
                } else {
                    SessionRepository<?> sessionRepository = (SessionRepository<?>) request.getAttribute(SessionRepositoryFilter.SESSION_REPOSITORY_ATTR);
                    if (Objects.nonNull(sessionRepository)) {
                        sessionRepository.deleteById(sessionId);
                    }
                }
            }
        });
    }

    private static Capabilities getUserAgent() {
        Optional<HttpServletRequest> request = getRequest();
        return request.map(httpServletRequest -> USER_AGENT_PARSER.parse(httpServletRequest.getHeader(REQ_HEADER_USER_AGENT))).orElse(null);
    }

    private static String concatIpRegion(String[] ipRegionParts) {
        return Stream.of(ipRegionParts).filter(ipRegionPart -> !"0".equals(ipRegionPart)).collect(Collectors.joining("-"));
    }
}
