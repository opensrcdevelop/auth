package cn.opensrcdevelop.auth.client.service;

import cn.opensrcdevelop.auth.client.authorize.AuthorizeExpressionRootObject;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.auth.client.config.AuthClientProperties;
import cn.opensrcdevelop.auth.client.constants.ApiConstants;
import cn.opensrcdevelop.auth.client.constants.AuthClientConstants;
import cn.opensrcdevelop.auth.client.exception.AuthClientException;
import cn.opensrcdevelop.auth.client.support.OAuth2Context;
import cn.opensrcdevelop.auth.client.support.OAuth2ContextHolder;
import cn.opensrcdevelop.auth.client.util.HttpUtil;
import cn.opensrcdevelop.auth.client.util.SpringELUtil;
import jakarta.servlet.http.HttpServletRequest;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.util.Assert;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;
import java.util.stream.IntStream;

/**
 * 客户端鉴权服务
 */
@Slf4j
@RequiredArgsConstructor
public class PermissionService implements ApplicationContextAware {

    private static final String API_USER_PERMISSIONS = "/api/v1/permission/me";
    private static final String HEADER_AUTHORIZATION = "Authorization";
    private static final String HEADER_BEARER = "Bearer ";
    private static final String URL_FORMAT = "%s://%s";

    private final AuthClientProperties authClientProperties;

    private ApplicationContext applicationContext;

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

        String[] needPermissions = authorizeAnno.value();
        boolean anyMatch = authorizeAnno.anyMatch();
        return anyMatch ? hasAnyPermission(methodInvocation, needPermissions) : hasAllPermission(methodInvocation, needPermissions);
    }

    /**
     * 判断用户是否拥有请求权限中的任意权限
     *
     * @param mi 方法调用
     * @param permissions 请求权限
     * @return 判断结果
     */
    private boolean hasAnyPermission(MethodInvocation mi, String... permissions) {
        try {
            // 1. 获取用户权限
            var userPermissions = getUserPermissions();

            // 2. 鉴权
            if (CollectionUtils.isNotEmpty(userPermissions)) {
                return Arrays.stream(permissions).anyMatch(p -> checkPermission(mi, p, userPermissions));
            }
            return false;
        } catch (HttpClientErrorException.Unauthorized e) {
            throw new OAuth2AuthenticationException("invalid provided token");
        } catch (IOException e) {
            throw new AuthClientException(e, "check permission failed");
        }
    }

    /**
     * 判断用户是否拥有全部的请求权限
     *
     * @param  mi 方法调用
     * @param permissions 请求权限
     * @return 判断结果
     */
    private boolean hasAllPermission(MethodInvocation mi, String... permissions) {
        try {
            // 1. 获取用户权限
            var userPermissions = getUserPermissions();

            // 2. 鉴权
            if (CollectionUtils.isNotEmpty(userPermissions)) {
                return Arrays.stream(permissions).allMatch(p -> checkPermission(mi, p, userPermissions));
            }
            return false;
        } catch (HttpClientErrorException.Unauthorized e) {
            throw new OAuth2AuthenticationException("invalid provided token");
        } catch (IOException e) {
            throw new AuthClientException(e, "check permission failed");
        }
    }

    /**
     * 权限检验
     *
     * @param mi 方法调用
     * @param permission 请求的权限
     * @param userPermissions 用户拥有的权限
     * @return 是否通过
     */
    private boolean checkPermission(MethodInvocation mi, String permission, List<Map<String, Object>> userPermissions) {
        // 1. 获取 properties 中设置的权限信息
        AuthClientProperties.ResourcePermission resourcePermission = authClientProperties.getAuthorize().get(permission);
        Assert.notNull(resourcePermission, "can not find permission in properties");

        // 2. 过滤用户拥有的权限信息
        var targetPermission = userPermissions.stream().filter(p ->
                StringUtils.equals((String) p.get(ApiConstants.RESOURCE_CODE), resourcePermission.getResource())
                && StringUtils.equals((String) p.get(ApiConstants.PERMISSION_CODE), resourcePermission.getPermission())
        ).findFirst();

        // 3. 用户拥有的权限与请求的权限匹配
        if (targetPermission.isPresent()) {
            // 3.1 限制条件校验
            @SuppressWarnings("unchecked")
            var conditions = (List<Map<String, Object>>) targetPermission.get().get(ApiConstants.CONDITIONS);
            if (CollectionUtils.isNotEmpty(conditions)) {
                // 3.1.1 需满足所有限制条件
                return conditions.stream().allMatch(c -> checkCondition(mi, (String) c.get(ApiConstants.EXPRESSION)));
            }
            return true;
        }
        return false;
    }

    /**
     * 限制条件校验
     *
     * @param mi 方法调用
     * @param expression SpringEL 表达式
     * @return 是否满足限制条件
     */
    private boolean checkCondition(MethodInvocation mi, String expression) {
        OAuth2Context oAuth2Context = OAuth2ContextHolder.getContext();
        if (Objects.isNull(oAuth2Context)) {
            return false;
        }
        Map<String, Object> attributes = new HashMap<>(oAuth2Context.getOAuth2Attributes().getAttributes());
        // 添加方法调用
        attributes.put(AuthClientConstants.METHOD_INVOCATION, mi);
        // 添加方法调用参数
        attributes.put(AuthClientConstants.METHOD_PARAMS, getMethodParams(mi));
        return Boolean.TRUE.equals(SpringELUtil.parseAuthorizeCondition(attributes, applicationContext, expression));
    }

    /**
     * 获取用户权限
     *
     * @return 用户权限
     */
    private List<Map<String, Object>> getUserPermissions() throws MalformedURLException {
        if (OAuth2ContextHolder.getContext() == null) {
            return Collections.emptyList();
        }

        // 1. 调用 API
        ServletRequestAttributes requestAttributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (Objects.isNull(requestAttributes)) {
            return Collections.emptyList();
        }
        HttpServletRequest request = requestAttributes.getRequest();

        // 1.1 获取 baseUrl
        String baseUrl;
        if (StringUtils.isEmpty(authClientProperties.getIssuer())) {
            URL url = new URL(request.getRequestURL().toString());
            baseUrl = String.format(URL_FORMAT, url.getProtocol(), url.getAuthority());
        } else {
            baseUrl = authClientProperties.getIssuer();
        }
        var apiResponse = HttpUtil.getRestClient().get()
                .uri(baseUrl + API_USER_PERMISSIONS)
                .header(HEADER_AUTHORIZATION, HEADER_BEARER + OAuth2ContextHolder.getContext().getAccessToken().getTokenValue())
                .retrieve()
                .body(new ParameterizedTypeReference<Map<String, Object>>() {});

        if (MapUtils.isEmpty(apiResponse)) {
            return Collections.emptyList();
        }

        // 2. 获取用户权限
        Boolean isSuccess = HttpUtil.getApiResponseItem(apiResponse, ApiConstants.SUCCESS);
        if (!Boolean.TRUE.equals(isSuccess)) {
            log.warn("获取用户权限失败。");
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

        Map<String, Object> methodParams = new LinkedHashMap<>(args.length);
        if (Objects.nonNull(paramNames) && args.length == paramNames.length ) {
            IntStream.range(0, args.length).forEach(i -> methodParams.put(paramNames[i], args[i]));
        }
        return methodParams;
    }

    @Override
    public void setApplicationContext(@NonNull ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }
}
