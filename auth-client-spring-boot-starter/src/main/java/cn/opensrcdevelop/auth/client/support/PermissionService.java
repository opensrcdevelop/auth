package cn.opensrcdevelop.auth.client.support;

import cn.opensrcdevelop.auth.client.config.AuthClientProperties;
import cn.opensrcdevelop.auth.client.constants.ApiConstants;
import cn.opensrcdevelop.auth.client.util.HttpUtil;
import cn.opensrcdevelop.auth.client.util.SpringELUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.util.Assert;
import org.springframework.web.client.HttpClientErrorException;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * 客户端鉴权服务
 */
@Slf4j
@RequiredArgsConstructor
public class PermissionService {

    private static final String API_USER_PERMISSIONS = "/api/v1/permission/me";
    private static final String HEADER_AUTHORIZATION = "Authorization";
    private static final String HEADER_BEARER = "Bearer ";

    private final AuthClientProperties authClientProperties;

    /**
     * 判断用户是否拥有请求权限中的任意权限
     *
     * @param permissions 请求权限
     * @return 判断结果
     */
    @SuppressWarnings("unused")
    public boolean hasAnyPermission(String... permissions) {
        try {
            // 1. 获取用户权限
            var userPermissions = getUserPermissions();

            // 2. 鉴权
            if (CollectionUtils.isNotEmpty(userPermissions)) {
                return Arrays.stream(permissions).anyMatch(p -> checkPermission(p, userPermissions));
            }
            return false;
        } catch (HttpClientErrorException.Unauthorized e) {
            throw new OAuth2AuthenticationException("Invalid provided token");
        }
    }

    /**
     * 判断用户是否拥有全部的请求权限
     *
     * @param permissions 请求权限
     * @return 判断结果
     */
    @SuppressWarnings("unused")
    public boolean hasAllPermission(String... permissions) {
        try {
            // 1. 获取用户权限
            var userPermissions = getUserPermissions();

            // 2. 鉴权
            if (CollectionUtils.isNotEmpty(userPermissions)) {
                return Arrays.stream(permissions).allMatch(p -> checkPermission(p, userPermissions));
            }
            return false;
        } catch (HttpClientErrorException.Unauthorized e) {
            throw new OAuth2AuthenticationException("Invalid provided token");
        }
    }

    /**
     * 权限检验
     *
     * @param permission 请求的权限
     * @param userPermissions 用户拥有的权限
     * @return 是否通过
     */
    private boolean checkPermission(String permission, List<Map<String, Object>> userPermissions) {
        // 1. 获取 properties 中设置的权限信息
        AuthClientProperties.ResourcePermission resourcePermission = authClientProperties.getAuthorize().get(permission);
        Assert.notNull(resourcePermission, "Can not find permission in properties");

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
                return conditions.stream().allMatch(c -> checkCondition((String) c.get(ApiConstants.EXPRESSION)));
            }
            return true;
        }
        return false;
    }

    /**
     * 限制条件校验
     *
     * @param expression SpringEL 表达式
     * @return 是否满足限制条件
     */
    public boolean checkCondition(String expression) {
        return Boolean.TRUE.equals(SpringELUtil.parseAuthorizeCondition(OAuth2ContextHolder.getContext().getUserAttributes().getAttributes(), expression));
    }

    /**
     * 获取用户权限
     *
     * @return 用户权限
     */
    private List<Map<String, Object>> getUserPermissions() {
        if (OAuth2ContextHolder.getContext() == null) {
            return Collections.emptyList();
        }

        // 1. 调用 API
        var apiResponse = HttpUtil.getRestClient().get()
                .uri(authClientProperties.getIssuer() + API_USER_PERMISSIONS)
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
}
