package cn.opensrcdevelop.auth.biz.util;

import cn.opensrcdevelop.auth.biz.constants.DataFilterEnum;
import cn.opensrcdevelop.auth.biz.constants.UserAttrDataTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.DataFilterRequestDto;
import cn.opensrcdevelop.auth.biz.entity.Role;
import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.jackson2.CoreJackson2Module;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.OAuth2ErrorCodes;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.authorization.authentication.OAuth2ClientAuthenticationToken;
import org.springframework.security.oauth2.server.authorization.client.RegisteredClient;
import org.springframework.security.oauth2.server.authorization.jackson2.OAuth2AuthorizationServerJackson2Module;
import org.springframework.security.oauth2.server.resource.introspection.OAuth2IntrospectionAuthenticatedPrincipal;
import org.springframework.security.web.jackson2.WebServletJackson2Module;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 认证工具
 */
public class AuthUtil {

    private AuthUtil() {}

    public static final ObjectMapper AUTH_OBJECT_MAPPER = new ObjectMapper();

    static {
        AUTH_OBJECT_MAPPER.registerModules(
                new CoreJackson2Module(),
                new WebServletJackson2Module(),
                new JavaTimeModule(),
                new OAuth2AuthorizationServerJackson2Module());
        AUTH_OBJECT_MAPPER.registerModule(new OAuth2AuthorizationServerJackson2Module());
    }

    /**
     * 获取当前用户名
     *
     * @return 当前用户名
     */
    public static Optional<String> getCurrentUsername() {
        return Optional.ofNullable(getCurrentJwtClaim(CommonConstants.USERNAME));
    }

    /**
     * 获取当前 Jwt claims
     *
     * @return 当前 Jwt claims
     */
    public static Map<String, Object> getCurrentJwtClaims() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return Collections.emptyMap();
        }

        if (authentication.getPrincipal() instanceof Jwt jwt) {
            return jwt.getClaims();
        }

        if (authentication.getPrincipal() instanceof OAuth2IntrospectionAuthenticatedPrincipal authenticatedPrincipal) {
            return authenticatedPrincipal.getClaims();
        }
        return Collections.emptyMap();
    }

    /**
     * 获取当前 Jwt claim
     *
     * @param claim claim
     * @return 值
     * @param <T> T
     */
    public static <T> T getCurrentJwtClaim(String claim) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return null;
        }

        if (authentication.getPrincipal() instanceof Jwt jwt) {
            return jwt.getClaim(claim);
        }

        if (authentication.getPrincipal() instanceof OAuth2IntrospectionAuthenticatedPrincipal authenticatedPrincipal) {
            return authenticatedPrincipal.getClaim(claim);
        }
        return null;
    }

    /**
     * 从认证信息中获取客户端认证 token
     *
     * @param authentication 认证信息
     * @return 客户端认证 token
     */
    public static OAuth2ClientAuthenticationToken getAuthenticatedClientElseThrowInvalidClient(Authentication authentication) {
        OAuth2ClientAuthenticationToken clientPrincipal = null;
        if (OAuth2ClientAuthenticationToken.class.isAssignableFrom(authentication.getPrincipal().getClass())) {
            clientPrincipal = (OAuth2ClientAuthenticationToken) authentication.getPrincipal();
        }

        if (clientPrincipal != null && clientPrincipal.isAuthenticated()) {
            return clientPrincipal;
        }
        throw new OAuth2AuthenticationException(OAuth2ErrorCodes.INVALID_CLIENT);
    }

    /**
     * 获取已授权的 scope
     *
     * @param registeredClient 客户端
     * @param requestedScopes 请求的 scope
     * @return 已授权的 scope
     */
    public static Set<String> getAuthorizedScopes(RegisteredClient registeredClient, Set<String> requestedScopes) {
        Set<String> authorizedScopes = registeredClient.getScopes();
        if (CollectionUtils.isNotEmpty(authorizedScopes)) {
            Set<String> unauthorizedScopes = CommonUtil.stream(requestedScopes).filter(s -> !registeredClient.getScopes().contains(s)).collect(Collectors.toSet());
            if (CollectionUtils.isNotEmpty(unauthorizedScopes)) {
                throw new OAuth2AuthenticationException(OAuth2ErrorCodes.INVALID_SCOPE);
            }

            if (CollectionUtils.isNotEmpty(requestedScopes)) {
                authorizedScopes = new LinkedHashSet<>(requestedScopes);
            } else {
                authorizedScopes = new LinkedHashSet<>();
            }
        }
        return authorizedScopes;
    }

    /**
     * Map 转 Json 字符串
     *
     * @param data Map
     * @return Json 字符串
     */
    public static String writeMap(Map<String, Object> data) {
        try {
            return AUTH_OBJECT_MAPPER.writeValueAsString(data);
        } catch (Exception ex) {
            throw new IllegalArgumentException(ex.getMessage(), ex);
        }
    }

    /**
     * Json 字符串转 Map
     *
     * @param data Json 字符串
     * @return Map
     */
    public static Map<String, Object> parseMap(String data) {
        try {
            return AuthUtil.AUTH_OBJECT_MAPPER.readValue(data, new TypeReference<>() {
            });
        } catch (Exception ex) {
            throw new IllegalArgumentException(ex.getMessage(), ex);
        }
    }

    /**
     * 将 User 转换为 Map（含扩展属性）
     *
     * @param user 用户
     * @return User Map
     */
    public static Map<String, Object> convertUserMap(User user) {
        if (user == null) {
            return Collections.emptyMap();
        }
        Map<String, Object> userMap = new HashMap<>();
        // 普通字段属性
        userMap.put(CommonUtil.extractFileNameFromGetter(User::getUserId), user.getUserId());
        userMap.put(CommonUtil.extractFileNameFromGetter(User::getUsername), user.getUsername());
        userMap.put(CommonUtil.extractFileNameFromGetter(User::getPhoneNumber), user.getPhoneNumber());
        userMap.put(CommonUtil.extractFileNameFromGetter(User::getEmailAddress), user.getEmailAddress());
        userMap.put(CommonUtil.extractFileNameFromGetter(User::getEnableMfa), user.getEnableMfa());
        userMap.put(CommonUtil.extractFileNameFromGetter(User::getLocked), user.getLocked());
        userMap.put(CommonUtil.extractFileNameFromGetter(User::getConsoleAccess), user.getConsoleAccess());
        userMap.put(CommonUtil.extractFileNameFromGetter(User::getCreateTime), user.getCreateTime().format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)));

        // 角色信息
        userMap.put(CommonConstants.ROLES, CommonUtil.stream(user.getRoles()).map(Role::getRoleCode).collect(Collectors.toCollection(ArrayList::new)));

        // 扩展字段属性
        CommonUtil.stream(user.getUserAttrs()).forEach(userAttr ->
                userMap.put(userAttr.getAttrKey(), convertUserAttrData(userAttr.getAttrValue(), UserAttrDataTypeEnum.valueOf(userAttr.getAttrDataType()))));
        return userMap;
    }

    /**
     * 编辑查询条件
     *
     * @param queryWrapper query
     * @param filter 过滤条件
     * @return 查询条件
     * @param <T> T
     */
    public static <T> QueryWrapper<T> editQuery(QueryWrapper<T> queryWrapper, DataFilterRequestDto filter) {
        DataFilterEnum filterType = DataFilterEnum.valueOf(filter.getFilterType());
        Boolean extFlg = filter.getExtFlg();
        boolean isExtFlag = Boolean.TRUE.equals(extFlg);
        String key = Boolean.TRUE.equals(extFlg) ? filter.getKey() : com.baomidou.mybatisplus.core.toolkit.StringUtils.camelToUnderline(filter.getKey()) ;
        Object value = convertDataFilterValue(filter.getValue(), UserAttrDataTypeEnum.valueOf(filter.getDataType()));

        // 布尔值类型的字段，基础字段在查询时转换为布尔类型，扩展字段在查询时保持字符串类型
        if (!isExtFlag && UserAttrDataTypeEnum.BOOLEAN.getType().equals(filter.getDataType()) && !(value instanceof Boolean)) {
            value = Boolean.valueOf(value.toString());
        }

        String queryPrefixAttrKey = "t3.attr_key";
        String queryPrefixAttrValue = "t2.attr_value";
        Object queryVal = value;
        return switch (filterType) {
            case EQ ->isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.eq(queryPrefixAttrValue, queryVal)) : queryWrapper.eq(key, queryVal);
            case NE -> isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.ne(queryPrefixAttrValue, queryVal)) : queryWrapper.ne(key, queryVal);
            case LIKE -> isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.like(queryPrefixAttrValue, queryVal)) : queryWrapper.like(key, queryVal);
            case NOT_LIKE -> isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.notLike(queryPrefixAttrValue, queryVal)) : queryWrapper.notLike(key, queryVal);
            case IN -> isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.in(queryPrefixAttrValue, queryVal)) : queryWrapper.in(key, queryVal);
            case NOT_IN -> isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.notIn(queryPrefixAttrValue, queryVal)) : queryWrapper.notIn(key, queryVal);
            case GT -> isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.gt(queryPrefixAttrValue, queryVal)) : queryWrapper.gt(key, queryVal);
            case GE -> isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.ge(queryPrefixAttrValue, queryVal)) : queryWrapper.ge(key, queryVal);
            case LT -> isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.lt(queryPrefixAttrValue, queryVal)) : queryWrapper.lt(key, queryVal);
            case LE -> isExtFlag ? queryWrapper.eq(queryPrefixAttrKey, key).and(o -> o.le(queryPrefixAttrValue, queryVal)) : queryWrapper.le(key, queryVal);
        };
    }

    /**
     * 转换用户扩展属性值
     *
     * @param value 用户扩展属性值
     * @param dataType 数据类型
     * @return 用户扩展属性值
     */
    public static Object convertUserAttrData(String value, UserAttrDataTypeEnum dataType) {
        return switch (dataType) {
            case NUMBER -> new BigDecimal(value);
            case BOOLEAN -> Boolean.valueOf(value);
            case DATETIME -> Long.parseLong(value);
            default -> value;
        };
    }

    private static Object convertDataFilterValue(Object value, UserAttrDataTypeEnum dataType) {
        return  switch (dataType) {
            case DATETIME -> Timestamp.from(Instant.ofEpochMilli(Long.parseLong(value.toString())));
            case STRING -> value.toString();
            default -> value;
        };
    }
}
