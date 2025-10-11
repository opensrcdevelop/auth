package cn.opensrcdevelop.auth.client.service;

import cn.opensrcdevelop.auth.client.authorize.AuthorizeExpressionRootObject;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.auth.client.config.AuthClientProperties;
import cn.opensrcdevelop.auth.client.constants.ApiConstants;
import cn.opensrcdevelop.auth.client.support.OAuth2ContextHolder;
import cn.opensrcdevelop.auth.client.util.HttpUtil;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.lang.reflect.Method;
import java.net.URI;
import java.util.*;
import java.util.stream.IntStream;

/**
 * 客户端鉴权服务
 */
@Slf4j
@RequiredArgsConstructor
public class PermissionService {

    private static final String API_VERIFY_PERMISSIONS = "/api/v1/permission/verify";
    private static final String HEADER_AUTHORIZATION = "Authorization";
    private static final String HEADER_BEARER = "Bearer ";
    private static final String URL_FORMAT = "%s://%s";

    private final AuthClientProperties authClientProperties;

    /**
     * 权限校验
     *
     * @param rootObject SpringEL 表达式 root 对象
     * @return 校验结果
     */
    @SuppressWarnings("all")
    public boolean checkPermission(AuthorizeExpressionRootObject rootObject) {
        MethodInvocation methodInvocation = rootObject.getMethodInvocation();
        if (Objects.isNull(methodInvocation)) {
            return true;
        }

        Authorize authorizeAnno = null;
        if (methodInvocation.getMethod().isAnnotationPresent(Authorize.class)) {
            authorizeAnno = methodInvocation.getMethod().getAnnotation(Authorize.class);
        } else if (methodInvocation.getMethod().getDeclaringClass().isAnnotationPresent(Authorize.class)) {
            authorizeAnno = methodInvocation.getMethod().getDeclaringClass().getAnnotation(Authorize.class);
        }

        if (Objects.isNull(authorizeAnno)) {
            return true;
        }

        List<String> needPermissions = Arrays.asList(authorizeAnno.value());
        boolean anyMatch = authorizeAnno.anyMatch();

        // 1. 获取 properties 中设置的权限信息
        List<String> permissionLocators = needPermissions.stream()
                .map(p -> authClientProperties.getAuthorize().get(p).getPermission())
                .toList();

        // 2. 校验权限
        List<Map<String, Object>> verifyResults = callVerifyApi(permissionLocators, getMethodParams(methodInvocation));

        // 3. 判断是否校验通过
        if (CollectionUtils.isEmpty(verifyResults)) {
            return false;
        }
        if (anyMatch) {
            return verifyResults.stream().anyMatch(r -> Boolean.TRUE.equals(r.get(ApiConstants.ALLOW)));
        } else {
            return verifyResults.stream().allMatch(r -> Boolean.TRUE.equals(r.get(ApiConstants.ALLOW)));
        }
    }

    private List<Map<String, Object>> callVerifyApi(List<String> permissions, Map<String, Object> methodParams) {
        if (OAuth2ContextHolder.getContext() == null) {
            return Collections.emptyList();
        }

        HttpServletRequest originalRequest = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();

        // 1. 调用 API
        ServletRequestAttributes requestAttributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (Objects.isNull(requestAttributes)) {
            return Collections.emptyList();
        }
        HttpServletRequest request = requestAttributes.getRequest();

        // 1.1 获取 baseUrl
        String baseUrl;
        if (StringUtils.isEmpty(authClientProperties.getIssuer())) {
            URI uri = URI.create(request.getRequestURL().toString());
            baseUrl = String.format(URL_FORMAT, uri.getScheme(), uri.getAuthority());
        } else {
            baseUrl = authClientProperties.getIssuer();
        }

        // 1.2 调用
        var apiResponse = HttpUtil.getRestClient()
                .post()
                .uri(baseUrl + API_VERIFY_PERMISSIONS)
                .header(HEADER_AUTHORIZATION, HEADER_BEARER + OAuth2ContextHolder.getContext().getAccessToken().getTokenValue())
                .header("X-Forwarded-For", originalRequest.getRemoteAddr())
                .header("User-Agent", originalRequest.getHeader("User-Agent"))
                .body(Map.of(
                        ApiConstants.PERMISSIONS, permissions,
                        ApiConstants.CONTEXT, Map.of(
                                ApiConstants.REQ_PARAMS, methodParams
                        )
                ))
                .retrieve()
                .body(new ParameterizedTypeReference<Map<String, Object>>() {});

        if (MapUtils.isEmpty(apiResponse)) {
            return Collections.emptyList();
        }

        // 2. 获取权限校验结果
        Boolean isSuccess = HttpUtil.getApiResponseItem(apiResponse, ApiConstants.SUCCESS);
        if (!Boolean.TRUE.equals(isSuccess)) {
            log.warn("校验用户权限失败。");
            return Collections.emptyList();
        }

        return HttpUtil.getApiResponseItem(apiResponse, ApiConstants.DATA);
    }

    /**
     * 获取方法调用参数
     *
     * @param mi 方法调用
     * @return 方法调用参数
     */
    private Map<String, Object> getMethodParams(MethodInvocation mi) {
        DefaultParameterNameDiscoverer parameterNameDiscoverer = new DefaultParameterNameDiscoverer();
        Object[] args = mi.getArguments();
        Method method = mi.getMethod();
        String[] paramNames = parameterNameDiscoverer.getParameterNames(method);

        Map<String, Object> methodParams = LinkedHashMap.newLinkedHashMap(args.length);
        if (Objects.nonNull(paramNames) && args.length == paramNames.length ) {
            IntStream.range(0, args.length).forEach(i -> methodParams.put(paramNames[i], args[i]));
        }
        return methodParams;
    }
}
