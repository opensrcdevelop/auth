package cn.opensrcdevelop.auth.biz.component.oauth2login;

import cn.opensrcdevelop.auth.biz.dto.identity.RequestConfigRequestDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceProvider;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.auth.biz.util.HttpExpressionUtil;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.HttpUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.security.oauth2.client.endpoint.OAuth2AccessTokenResponseClient;
import org.springframework.security.oauth2.client.endpoint.OAuth2AuthorizationCodeGrantRequest;
import org.springframework.security.oauth2.client.endpoint.RestClientAuthorizationCodeTokenResponseClient;
import org.springframework.security.oauth2.client.http.OAuth2ErrorResponseErrorHandler;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.core.ClientAuthenticationMethod;
import org.springframework.security.oauth2.core.endpoint.OAuth2AccessTokenResponse;
import org.springframework.security.oauth2.core.endpoint.OAuth2ParameterNames;
import org.springframework.security.oauth2.core.http.converter.OAuth2AccessTokenResponseHttpMessageConverter;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriComponentsBuilder;

@Component
@RequiredArgsConstructor
public class CustomAuthorizationCodeTokenResponseClient
        implements
            OAuth2AccessTokenResponseClient<OAuth2AuthorizationCodeGrantRequest> {

    private final IdentitySourceRegistrationService identitySourceRegistrationService;
    private final RestClientAuthorizationCodeTokenResponseClient delegate = new RestClientAuthorizationCodeTokenResponseClient();
    private final CustomMapOAuth2AccessTokenResponseConverter accessTokenResponseConverter = new CustomMapOAuth2AccessTokenResponseConverter();

    private final RestClient restClient = RestClient
            .builder()
            .requestInterceptor(new HttpUtil.CustomClientHttpRequestInterceptor())
            .defaultStatusHandler(new OAuth2ErrorResponseErrorHandler())
            .messageConverters(messageConverters -> {
                messageConverters.clear();
                var accessTokenResponseHttpMessageConverter = new OAuth2AccessTokenResponseHttpMessageConverter();
                accessTokenResponseHttpMessageConverter.setAccessTokenResponseConverter(accessTokenResponseConverter);
                messageConverters.addFirst(accessTokenResponseHttpMessageConverter);
                messageConverters.add(new FormHttpMessageConverter());
                messageConverters.add(new MappingJackson2HttpMessageConverter());
            })
            .build();

    @Override
    public OAuth2AccessTokenResponse getTokenResponse(OAuth2AuthorizationCodeGrantRequest authorizationGrantRequest) {
        OAuth2AccessTokenResponse accessTokenResponse;

        // 1. 获取身份源提供商
        ClientRegistration clientRegistration = authorizationGrantRequest.getClientRegistration();
        IdentitySourceRegistration identitySourceRegistration = identitySourceRegistrationService
                .getByCode(clientRegistration.getRegistrationId());
        IdentitySourceProvider identitySourceProvider = identitySourceRegistration.getIdentitySourceProvider();

        // 2. 是否启用自定义令牌请求
        if (Boolean.TRUE.equals(identitySourceProvider.getEnableCustomTokenReq())) {
            // 2.1 自定义请求

            // 2.1.1 验证客户端认证方式
            validateClientAuthenticationMethod(authorizationGrantRequest);

            // 2.1.2 填充请求配置
            Map<String, Object> valContext = getRequestCfgValueContext(authorizationGrantRequest,
                    identitySourceRegistration);
            String requestCfgStr = CommonUtil.fillTemplate(identitySourceProvider.getTokenReqCfg(), valContext);

            // 2.1.3 请求配置反序列化
            RequestConfigRequestDto requestCfg = CommonUtil.deserializeObject(requestCfgStr,
                    RequestConfigRequestDto.class);

            // 2.1.4 执行 SpEL 表达式
            HttpExpressionUtil.parseSpELMap(requestCfg.getParams());
            HttpExpressionUtil.parseSpELMap(requestCfg.getBody());
            HttpExpressionUtil.parseSpELMap(requestCfg.getHeaders());
            HttpExpressionUtil.parseSpELMap(requestCfg.getPathVariables());

            // 2.1.5 设置 access_token 属性名称
            if (StringUtils.isNotEmpty(requestCfg.getAccessTokenAttr())) {
                accessTokenResponseConverter.setAccessTokenAttrName(requestCfg.getAccessTokenAttr());
            } else {
                accessTokenResponseConverter.setAccessTokenAttrName(OAuth2ParameterNames.ACCESS_TOKEN);
            }

            // 2.1.6 发送请求
            accessTokenResponse = getResponse(clientRegistration, identitySourceProvider, requestCfg);
        } else {
            // 2.2 默认请求
            accessTokenResponse = delegate.getTokenResponse(authorizationGrantRequest);
        }

        return accessTokenResponse;
    }

    private void validateClientAuthenticationMethod(OAuth2AuthorizationCodeGrantRequest grantRequest) {
        ClientRegistration clientRegistration = grantRequest.getClientRegistration();
        ClientAuthenticationMethod clientAuthenticationMethod = clientRegistration.getClientAuthenticationMethod();
        boolean supportedClientAuthenticationMethod = clientAuthenticationMethod.equals(ClientAuthenticationMethod.NONE)
                || clientAuthenticationMethod.equals(ClientAuthenticationMethod.CLIENT_SECRET_BASIC)
                || clientAuthenticationMethod.equals(ClientAuthenticationMethod.CLIENT_SECRET_POST);
        if (!supportedClientAuthenticationMethod) {
            throw new IllegalArgumentException(String.format(
                    "This class supports `client_secret_basic`, `client_secret_post`, and `none` by default. Client [%s] is using [%s] instead. Please use a supported client authentication method, or use `set/addParametersConverter` or `set/addHeadersConverter` to supply an instance that supports [%s].",
                    clientRegistration.getRegistrationId(), clientAuthenticationMethod, clientAuthenticationMethod));
        }
    }

    private OAuth2AccessTokenResponse getResponse(ClientRegistration clientRegistration,
            IdentitySourceProvider identitySourceProvider, RequestConfigRequestDto requestCfg) {
        String tokenUri = identitySourceProvider.getTokenUri();
        if (HttpMethod.GET.matches(requestCfg.getMethod())) {
            UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(tokenUri);
            if (MapUtils.isNotEmpty(requestCfg.getParams())) {
                requestCfg.getParams().forEach(uriBuilder::queryParam);
            }

            if (MapUtils.isNotEmpty(requestCfg.getPathVariables())) {
                uriBuilder.buildAndExpand(requestCfg.getPathVariables());
            }
            tokenUri = uriBuilder.toUriString();
        }

        return restClient.method(HttpMethod.valueOf(requestCfg.getMethod()))
                .uri(tokenUri)
                .headers(headers -> headers.addAll(convert2HttpHeaders(requestCfg.getHeaders(), clientRegistration)))
                .body(requestCfg.getBody())
                .retrieve()
                .body(OAuth2AccessTokenResponse.class);
    }

    private Map<String, Object> getRequestCfgValueContext(
            OAuth2AuthorizationCodeGrantRequest authorizationCodeGrantRequest,
            IdentitySourceRegistration identitySourceRegistration) {
        Map<String, Object> requestCfgValueContext = new HashMap<>();

        // 请求上下文参数
        requestCfgValueContext.put(OAuth2ParameterNames.CLIENT_ID, identitySourceRegistration.getClientId());
        requestCfgValueContext.put(OAuth2ParameterNames.CLIENT_SECRET, identitySourceRegistration.getClientSecret());
        requestCfgValueContext.put(OAuth2ParameterNames.GRANT_TYPE,
                authorizationCodeGrantRequest.getGrantType().getValue());
        requestCfgValueContext.put(OAuth2ParameterNames.CODE,
                authorizationCodeGrantRequest.getAuthorizationExchange().getAuthorizationResponse().getCode());
        requestCfgValueContext.put(OAuth2ParameterNames.REDIRECT_URI,
                authorizationCodeGrantRequest.getAuthorizationExchange().getAuthorizationRequest().getRedirectUri());

        // 额外参数
        if (StringUtils.isNotBlank(identitySourceRegistration.getAdditionalParams())) {
            requestCfgValueContext.putAll(CommonUtil.deserializeObject(identitySourceRegistration.getAdditionalParams(),
                    new TypeReference<Map<String, Object>>() {
                    }));
        }
        return requestCfgValueContext;
    }

    private HttpHeaders convert2HttpHeaders(Map<String, Object> requestHeaders, ClientRegistration clientRegistration) {
        HttpHeaders headers = new HttpHeaders();
        headers.setAccept(List.of(MediaType.APPLICATION_JSON));
        if (ClientAuthenticationMethod.CLIENT_SECRET_BASIC.equals(clientRegistration.getClientAuthenticationMethod())) {
            String clientId = URLEncoder.encode(clientRegistration.getClientId(), StandardCharsets.UTF_8);
            String clientSecret = URLEncoder.encode(clientRegistration.getClientId(), StandardCharsets.UTF_8);
            headers.setBasicAuth(clientId, clientSecret);
        }

        if (MapUtils.isNotEmpty(requestHeaders)) {
            requestHeaders.forEach((key, value) -> headers.add(key, value != null ? value.toString() : null));
        }

        return headers;
    }
}
