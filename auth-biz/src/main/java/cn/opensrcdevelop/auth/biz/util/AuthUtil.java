package cn.opensrcdevelop.auth.biz.util;

import cn.opensrcdevelop.auth.biz.constants.ConjunctionType;
import cn.opensrcdevelop.auth.biz.constants.DataFilterEnum;
import cn.opensrcdevelop.auth.biz.constants.UserAttrDataTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.entity.role.Role;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.jackson2.CoreJackson2Module;
import org.springframework.security.oauth2.client.jackson2.OAuth2ClientJackson2Module;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.OAuth2ErrorCodes;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
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

    private AuthUtil() {
    }

    public static final ObjectMapper AUTH_OBJECT_MAPPER = new ObjectMapper();

    static {
        AUTH_OBJECT_MAPPER.registerModules(
                new CoreJackson2Module(),
                new WebServletJackson2Module(),
                new JavaTimeModule(),
                new OAuth2ClientJackson2Module(),
                new OAuth2AuthorizationServerJackson2Module());
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
     * 获取当前用户 ID
     *
     * @return 当前用户 ID
     */
    public static String getCurrentUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication.getPrincipal() instanceof User user) {
            return user.getUserId();
        }
        return getCurrentJwtClaim(JwtClaimNames.SUB);
    }

    /**
     * 获取当前客户端 ID
     *
     * @return 当前客户端 ID
     */
    public static Optional<String> getCurrentClientId() {
        List<String> aud = getCurrentJwtClaim(JwtClaimNames.AUD);
        return Optional.ofNullable(CollectionUtils.isNotEmpty(aud) ? aud.getFirst() : null);
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
     * @param claim
     *            claim
     * @return 值
     * @param <T>
     *            T
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
     * @param authentication
     *            认证信息
     * @return 客户端认证 token
     */
    public static OAuth2ClientAuthenticationToken getAuthenticatedClientElseThrowInvalidClient(
            Authentication authentication) {
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
     * @param registeredClient
     *            客户端
     * @param requestedScopes
     *            请求的 scope
     * @return 已授权的 scope
     */
    public static Set<String> getAuthorizedScopes(RegisteredClient registeredClient, Set<String> requestedScopes) {
        Set<String> authorizedScopes = registeredClient.getScopes();
        if (CollectionUtils.isNotEmpty(authorizedScopes)) {
            Set<String> unauthorizedScopes = CommonUtil.stream(requestedScopes)
                    .filter(s -> !registeredClient.getScopes().contains(s)).collect(Collectors.toSet());
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
     * @param data
     *            Map
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
     * @param data
     *            Json 字符串
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
     * @param user
     *            用户
     * @return User Map
     */
    public static Map<String, Object> convertUserMap(User user) {
        return convertUserMap(user, false, false);
    }

    /**
     * 将 User 转换为 Map（含扩展属性）
     *
     * @param user
     *            用户
     * @param withDictDataId
     *            字典类型返回字典数据ID
     * @param withTimestamp
     *            日期 / 日期时间类型返回时间戳
     * @return User Map
     */
    public static Map<String, Object> convertUserMap(User user, boolean withDictDataId, boolean withTimestamp) {
        if (user == null) {
            return Collections.emptyMap();
        }
        Map<String, Object> userMap = new LinkedHashMap<>();
        // 普通字段属性
        userMap.put(CommonUtil.extractFieldNameFromGetter(User::getUserId), user.getUserId());
        userMap.put(CommonUtil.extractFieldNameFromGetter(User::getUsername), user.getUsername());
        userMap.put(CommonUtil.extractFieldNameFromGetter(User::getPhoneNumber), user.getPhoneNumber());
        userMap.put(CommonUtil.extractFieldNameFromGetter(User::getEmailAddress), user.getEmailAddress());
        userMap.put(CommonUtil.extractFieldNameFromGetter(User::getEnableMfa), user.getEnableMfa());
        userMap.put(CommonUtil.extractFieldNameFromGetter(User::getLocked), user.getLocked());
        userMap.put(CommonUtil.extractFieldNameFromGetter(User::getConsoleAccess), user.getConsoleAccess());
        userMap.put(CommonUtil.extractFieldNameFromGetter(User::getCreateTime), user.getCreateTime()
                .format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)));

        // 角色信息
        userMap.put(CommonConstants.ROLES, CommonUtil.stream(user.getRoles()).map(Role::getRoleCode)
                .collect(Collectors.toCollection(ArrayList::new)));

        // 扩展字段属性
        CommonUtil.stream(user.getUserAttrs())
                .forEach(userAttr -> userMap.put(userAttr.getAttrKey(), convertUserAttrData(userAttr.getAttrValue(),
                        UserAttrDataTypeEnum.valueOf(userAttr.getAttrDataType()), withDictDataId, withTimestamp)));
        return userMap;
    }

    /**
     * 转换用户扩展属性值
     *
     * @param value
     *            用户扩展属性值
     * @param dataType
     *            数据类型
     * @return 用户扩展属性值
     */
    public static Object convertUserAttrData(String value, UserAttrDataTypeEnum dataType) {
        return convertUserAttrData(value, dataType, false, false);
    }

    /**
     * 转换用户扩展属性值
     *
     * @param value
     *            用户扩展属性值
     * @param dataType
     *            数据类型
     * @param withDictDataId
     *            字典类型返回字典数据ID
     * @param withTimestamp
     *            日期 / 日期时间类型返回时间戳
     * @return 用户扩展属性值
     */
    public static Object convertUserAttrData(String value, UserAttrDataTypeEnum dataType, boolean withDictDataId,
            boolean withTimestamp) {
        if (Objects.isNull(value)) {
            return null;
        }

        return switch (dataType) {
            case NUMBER -> new BigDecimal(value);
            case BOOLEAN -> Boolean.valueOf(value);
            case DATETIME -> withTimestamp
                    ? Long.parseLong(value)
                    : CommonUtil.convertTimestamp2String(Long.parseLong(value),
                            CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS);
            case DATE -> withTimestamp
                    ? Long.parseLong(value)
                    : CommonUtil.convertTimestamp2String(Long.parseLong(value),
                            CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDD);
            case DICT -> withDictDataId
                    ? SpringContextUtil.getBean(DictDataService.class).detail(value).getId()
                    : SpringContextUtil.getBean(DictDataService.class).detail(value).getLabel();
            default -> value;
        };
    }

    /**
     * 编辑查询条件
     *
     * @param queryWrapper
     *            query
     * @param filter
     *            过滤条件
     * @param conjunction
     *            连接符
     * @param <T>
     *            T
     */
    public static <T> void editQuery(QueryWrapper<T> queryWrapper, DataFilterDto filter, ConjunctionType conjunction) {
        DataFilterEnum filterType = DataFilterEnum.valueOf(filter.getFilterType());
        UserAttrDataTypeEnum dataType = UserAttrDataTypeEnum.valueOf(filter.getDataType());
        if (Boolean.TRUE.equals(filter.getExtFlg())) {
            editExistsQuery(queryWrapper, filterType, filter.getKey(), filter.getValue(), dataType, conjunction);
        } else {
            editQueryCondition(queryWrapper, filterType, filter.getKey(), filter.getValue(), dataType, conjunction);
        }
    }

    private static <T> void editExistsQuery(QueryWrapper<T> queryWrapper,
            DataFilterEnum filterType,
            String attrKey, Object value,
            UserAttrDataTypeEnum valueDataType,
            ConjunctionType conjunction) {
        String sqlSegment = """
                SELECT
                    1
                FROM
                    t_user_attr_mapping,
                    t_user_attr
                WHERE
                    t_user_attr_mapping.attr_id = t_user_attr.attr_id
                    AND
                    t_user_attr_mapping.user_id = t_user.user_id
                    AND
                    %s
                """;
        if (ConjunctionType.OR == conjunction) {
            queryWrapper.or(q -> q.exists(String.format(sqlSegment,
                    getExistsConditionSqlSegment(filterType, attrKey, value, valueDataType))));
        } else {
            queryWrapper.and(q -> q.exists(String.format(sqlSegment,
                    getExistsConditionSqlSegment(filterType, attrKey, value, valueDataType))));
        }
    }

    private static String getExistsConditionSqlSegment(DataFilterEnum filterType, String attrKey, Object value,
            UserAttrDataTypeEnum valueDataType) {
        String queryKey = "t_user_attr.attr_key";
        String valueKey;
        // 日期、日期时间、数字类型进行 sql 类型强制转换为数值类型进行条件判断
        if (List.of(UserAttrDataTypeEnum.DATE, UserAttrDataTypeEnum.DATETIME, UserAttrDataTypeEnum.NUMBER)
                .contains(valueDataType)) {
            valueKey = "CASE WHEN t_user_attr_mapping.attr_value ~ '^\\d+(\\.\\d+)?$' THEN CAST(t_user_attr_mapping.attr_value AS NUMERIC) ELSE NULL END";
        } else {
            valueKey = "t_user_attr_mapping.attr_value";
        }

        // 构造查询条件
        QueryWrapper<Object> queryWrapper = new QueryWrapper<>();
        switch (filterType) {
            case EQ -> queryWrapper.eq(queryKey, attrKey).and(o -> o.eq(valueKey, value));
            case NE -> queryWrapper.eq(queryKey, attrKey).and(o -> o.ne(valueKey, value));
            case LIKE -> queryWrapper.eq(queryKey, attrKey).and(o -> o.like(valueKey, value));
            case NOT_LIKE -> queryWrapper.eq(queryKey, attrKey).and(o -> o.notLike(valueKey, value));
            case IN -> queryWrapper.eq(queryKey, attrKey).and(o -> o.in(valueKey,
                    CommonUtil.stream(Arrays.asList(value.toString().split(CommonConstants.COMMA)))
                            .filter(StringUtils::isNotEmpty).toList()));
            case NOT_IN -> queryWrapper.eq(queryKey, attrKey).and(o -> o.notIn(valueKey,
                    CommonUtil.stream(Arrays.asList(value.toString().split(CommonConstants.COMMA)))
                            .filter(StringUtils::isNotEmpty).toList()));
            case GT -> queryWrapper.eq(queryKey, attrKey).and(o -> o.gt(valueKey, value));
            case GE -> queryWrapper.eq(queryKey, attrKey).and(o -> o.ge(valueKey, value));
            case LT -> queryWrapper.eq(queryKey, attrKey).and(o -> o.lt(valueKey, value));
            case LE -> queryWrapper.eq(queryKey, attrKey).and(o -> o.le(valueKey, value));
        }

        // 替换参数
        String paramName1 = "#{ew.paramNameValuePairs.MPGENVAL1}";
        String paramName2 = "#{ew.paramNameValuePairs.MPGENVAL2}";
        String sqlSegment = queryWrapper.getSqlSegment();
        sqlSegment = sqlSegment.replace(paramName1, "'" + attrKey + "'");
        if (value instanceof Number) {
            sqlSegment = sqlSegment.replace(paramName2, value.toString());
        } else if (filterType.equals(DataFilterEnum.LIKE) || filterType.equals(DataFilterEnum.NOT_LIKE)) {
            sqlSegment = sqlSegment.replace(paramName2, "'%" + value.toString() + "%'");
        } else {
            sqlSegment = sqlSegment.replace(paramName2, "'" + value.toString() + "'");
        }
        return sqlSegment;
    }

    @SuppressWarnings({"java:S6541", "java:S3776"})
    private static <T> void editQueryCondition(QueryWrapper<T> queryWrapper,
            DataFilterEnum filterType,
            String attrKey, Object value,
            UserAttrDataTypeEnum valueDataType,
            ConjunctionType conjunction) {
        String queryKey = "t_user." + com.baomidou.mybatisplus.core.toolkit.StringUtils.camelToUnderline(attrKey);
        Object queryValue = switch (valueDataType) {
            case DATETIME, DATE -> Timestamp.from(Instant.ofEpochMilli(Long.parseLong(value.toString())));
            case BOOLEAN -> Boolean.valueOf(value.toString());
            default -> value;
        };

        // 构造查询条件
        boolean isOr = ConjunctionType.OR == conjunction;
        switch (filterType) {
            case EQ -> {
                if (isOr) {
                    queryWrapper.or(q -> q.eq(queryKey, queryValue));
                } else {
                    queryWrapper.eq(queryKey, queryValue);
                }
            }
            case NE -> {
                if (isOr) {
                    queryWrapper.or(q -> q.ne(queryKey, queryValue));
                } else {
                    queryWrapper.ne(queryKey, queryValue);
                }
            }
            case LIKE -> {
                if (isOr) {
                    queryWrapper.or(q -> q.like(queryKey, queryValue));
                } else {
                    queryWrapper.like(queryKey, queryValue);
                }
            }
            case NOT_LIKE -> {
                if (isOr) {
                    queryWrapper.or(q -> q.notLike(queryKey, queryValue));
                } else {
                    queryWrapper.notLike(queryKey, queryValue);
                }
            }
            case IN -> {
                if (isOr) {
                    queryWrapper.or(q -> q.in(queryKey,
                            CommonUtil.stream(Arrays.asList(queryValue.toString().split(CommonConstants.COMMA)))
                                    .filter(StringUtils::isNotEmpty).toList()));
                } else {
                    queryWrapper.in(queryKey,
                            CommonUtil.stream(Arrays.asList(queryValue.toString().split(CommonConstants.COMMA)))
                                    .filter(StringUtils::isNotEmpty).toList());
                }
            }
            case NOT_IN -> {
                if (isOr) {
                    queryWrapper.or(q -> q.notIn(queryKey,
                            CommonUtil.stream(Arrays.asList(queryValue.toString().split(CommonConstants.COMMA)))
                                    .filter(StringUtils::isNotEmpty).toList()));
                } else {
                    queryWrapper.notIn(queryKey,
                            CommonUtil.stream(Arrays.asList(queryValue.toString().split(CommonConstants.COMMA)))
                                    .filter(StringUtils::isNotEmpty).toList());
                }
            }
            case GT -> {
                if (isOr) {
                    queryWrapper.or(q -> q.gt(queryKey, queryValue));
                } else {
                    queryWrapper.gt(queryKey, queryValue);
                }
            }
            case GE -> {
                if (isOr) {
                    queryWrapper.or(q -> q.ge(queryKey, queryValue));
                } else {
                    queryWrapper.ge(queryKey, queryValue);
                }
            }
            case LT -> {
                if (isOr) {
                    queryWrapper.or(q -> q.lt(queryKey, queryValue));
                } else {
                    queryWrapper.lt(queryKey, queryValue);
                }
            }
            case LE -> {
                if (isOr) {
                    queryWrapper.or(q -> q.le(queryKey, queryValue));
                } else {
                    queryWrapper.le(queryKey, queryValue);
                }
            }
        }
    }
}
