package cn.opensrcdevelop.auth.biz.component;

import cn.opensrcdevelop.auth.biz.dto.identity.RequestConfigRequestDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceProvider;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.auth.biz.service.identity.ThirdAccountService;
import cn.opensrcdevelop.auth.biz.util.HttpExpressionUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.HttpUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateExceptionHandler;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.MapUtils;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserService;
import org.springframework.security.oauth2.core.*;
import org.springframework.security.oauth2.core.endpoint.OAuth2ParameterNames;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestOperations;
import org.springframework.web.util.UriComponentsBuilder;

import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.*;

@Component
@RequiredArgsConstructor
public class CustomOAuth2UserService implements OAuth2UserService<OAuth2UserRequest, OAuth2User> {

    private static final String INVALID_USER_INFO_RESPONSE_ERROR_CODE = "invalid_user_info_response";
    private static final MediaType DEFAULT_CONTENT_TYPE = MediaType.valueOf(MediaType.APPLICATION_FORM_URLENCODED_VALUE + ";charset=UTF-8");

    private final IdentitySourceRegistrationService identitySourceRegistrationService;
    private final ThirdAccountService thirdAccountService;
    private final RestOperations restOperations = new RestTemplateBuilder()
            .interceptors(new HttpUtil.CustomClientHttpRequestInterceptor()).build();

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) throws OAuth2AuthenticationException {
        List<Map<String, Object>> attributesList = new ArrayList<>();
        ClientRegistration clientRegistration = userRequest.getClientRegistration();
        IdentitySourceRegistration identitySourceRegistration = identitySourceRegistrationService.getByCode(clientRegistration.getRegistrationId());
        IdentitySourceProvider identitySourceProvider = identitySourceRegistration.getIdentitySourceProvider();

        String[] userInfoUris = StringUtils.delimitedListToStringArray(identitySourceProvider.getUserInfoUris(), CommonConstants.COMMA);
        Map<String, RequestConfigRequestDto> userInfoReqCfgMap = new HashMap<>();
        // 自定义用户信息请求
        if (Boolean.TRUE.equals(identitySourceProvider.getEnableCustomUserInfoReq()) && StringUtils.hasText(identitySourceProvider.getUserInfoReqCfg())) {
            // 填充请求配置
            String requestCfgStr = fillRequestCfg(identitySourceProvider.getUserInfoReqCfg(), getRequestCfgValueContext(userRequest));

            // 反序列化
            userInfoReqCfgMap = CommonUtil.deserializeObject(requestCfgStr, new TypeReference<Map<String, RequestConfigRequestDto>>() {});
        }

        // 获取用户信息
        for (String userInfoUri : userInfoUris) {
            RequestEntity<?> apiRequest;
            // 自定义用户信息请求
            if (userInfoReqCfgMap.containsKey(userInfoUri)) {
                RequestConfigRequestDto requestCfg = userInfoReqCfgMap.get(userInfoUri);
                // 执行 SpEL 表达式
                HttpExpressionUtil.parseSpELMap(requestCfg.getParams());
                HttpExpressionUtil.parseSpELMap(requestCfg.getPathVariables());
                HttpExpressionUtil.parseSpELMap(requestCfg.getHeaders());
                HttpExpressionUtil.parseSpELMap(requestCfg.getBody());

                apiRequest = getCustomRequestEntity(userInfoUri, requestCfg);
            } else {
                // 默认请求方式
                apiRequest = getRequestEntity(userInfoUri, userRequest.getAccessToken(), clientRegistration);
            }
            attributesList.addAll(getResponse(apiRequest));
        }

        // 绑定第三方账号
        return thirdAccountService.bind(attributesList, identitySourceRegistration);
    }

    private String fillRequestCfg(String requestCfg, Map<String, Object> requestCfgValueContext) {
        try (StringReader reader = new StringReader(requestCfg);
             StringWriter writer = new StringWriter()) {
            Configuration cfg = new Configuration(Configuration.DEFAULT_INCOMPATIBLE_IMPROVEMENTS);
            cfg.setTemplateExceptionHandler(TemplateExceptionHandler.IGNORE_HANDLER);
            Template processor = new Template(CommonUtil.getUUIDString(), reader, cfg, StandardCharsets.UTF_8.name());
            processor.process(requestCfgValueContext, writer);
            return writer.toString();
        } catch (Exception ex) {
            throw new ServerException(ex.getMessage(), ex);
        }
    }

    private Map<String, Object> getRequestCfgValueContext(OAuth2UserRequest userRequest) {
        Map<String, Object> requestCfgValueContext = new HashMap<>();
        requestCfgValueContext.put(OAuth2ParameterNames.ACCESS_TOKEN, userRequest.getAccessToken().getTokenValue());
        requestCfgValueContext.put(OAuth2ParameterNames.CLIENT_ID, userRequest.getClientRegistration().getClientId());
        requestCfgValueContext.put(OAuth2ParameterNames.CLIENT_SECRET, userRequest.getClientRegistration().getClientSecret());

        if (MapUtils.isNotEmpty(userRequest.getAdditionalParameters())) {
            requestCfgValueContext.putAll(userRequest.getAdditionalParameters());
        }
        return requestCfgValueContext;
    }

    private RequestEntity<?> getRequestEntity(String userInfoUri, OAuth2AccessToken accessToken, ClientRegistration clientRegistration) {
        HttpMethod httpMethod = getHttpMethod(clientRegistration);
        HttpHeaders headers = new HttpHeaders();
        headers.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));
        URI uri = UriComponentsBuilder
                .fromUriString(userInfoUri)
                .build()
                .toUri();

        RequestEntity<?> request;
        if (HttpMethod.POST.equals(httpMethod)) {
            headers.setContentType(DEFAULT_CONTENT_TYPE);
            MultiValueMap<String, String> formParameters = new LinkedMultiValueMap<>();
            formParameters.add(OAuth2ParameterNames.ACCESS_TOKEN, accessToken.getTokenValue());
            request = new RequestEntity<>(formParameters, headers, httpMethod, uri);
        } else if (AuthenticationMethod.QUERY.equals(clientRegistration.getProviderDetails().getUserInfoEndpoint().getAuthenticationMethod())) {
            return new RequestEntity<>(headers, httpMethod, UriComponentsBuilder.fromUri(uri).queryParam(OAuth2ParameterNames.ACCESS_TOKEN, accessToken.getTokenValue()).build().toUri());
        }
        else {
            headers.setBearerAuth(accessToken.getTokenValue());
            request = new RequestEntity<>(headers, httpMethod, uri);
        }

        return request;
    }

    private RequestEntity<?> getCustomRequestEntity(String userInfoUri, RequestConfigRequestDto requestCfg) {
        String method = requestCfg.getMethod();
        UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromUriString(userInfoUri);
        if (MapUtils.isNotEmpty(requestCfg.getParams())) {
            requestCfg.getParams().forEach(uriBuilder::queryParam);
        }

        if (MapUtils.isNotEmpty(requestCfg.getPathVariables())) {
            uriBuilder.buildAndExpand(requestCfg.getPathVariables());
        }

        if (HttpMethod.GET.matches(method)) {
            return new RequestEntity<>(convert2MultiValueMap(requestCfg.getHeaders()), HttpMethod.GET, uriBuilder.build().toUri());
        }
        return new RequestEntity<>(requestCfg.getBody(), convert2MultiValueMap(requestCfg.getHeaders()), HttpMethod.valueOf(method),  uriBuilder.build().toUri());
    }

    private HttpMethod getHttpMethod(ClientRegistration clientRegistration) {
        if (AuthenticationMethod.FORM
                .equals(clientRegistration.getProviderDetails().getUserInfoEndpoint().getAuthenticationMethod())) {
            return HttpMethod.POST;
        }
        return HttpMethod.GET;
    }

    private MultiValueMap<String, String> convert2MultiValueMap(Map<String, Object> requestHeaders) {
        MultiValueMap<String, String> multiValueHeaders = new LinkedMultiValueMap<>();
        if (MapUtils.isNotEmpty(requestHeaders)) {
            requestHeaders.forEach((key, value) ->
                    multiValueHeaders.add(key, value != null ? value.toString() : null)
            );
        }
        return multiValueHeaders;
    }

    @SuppressWarnings("unchecked")
    private List<Map<String, Object>> getResponse(RequestEntity<?> request) {
        try {
            List<Map<String, Object>> responseObjList = new ArrayList<>();
            Object responseObj  = this.restOperations.exchange(request, Object.class).getBody();
            if (responseObj instanceof Map<?, ?>) {
                responseObjList.add((Map<String, Object>) responseObj);
            }

            if (responseObj instanceof List<?>) {
                responseObjList.addAll((List<Map<String, Object>>) responseObj);
            }
            return responseObjList;
        } catch (OAuth2AuthorizationException ex) {
            OAuth2Error oauth2Error = ex.getError();
            StringBuilder errorDetails = new StringBuilder();
            errorDetails.append("Error details: [");
            errorDetails.append("UserInfo Uri: ").append(request.getUrl());
            errorDetails.append(", Error Code: ").append(oauth2Error.getErrorCode());
            if (oauth2Error.getDescription() != null) {
                errorDetails.append(", Error Description: ").append(oauth2Error.getDescription());
            }
            errorDetails.append("]");
            oauth2Error = new OAuth2Error(INVALID_USER_INFO_RESPONSE_ERROR_CODE,
                    "An error occurred while attempting to retrieve the UserInfo Resource: " + errorDetails,
                    null);
            throw new OAuth2AuthenticationException(oauth2Error, oauth2Error.toString(), ex);
        } catch (RestClientException ex) {
            OAuth2Error oauth2Error = new OAuth2Error(INVALID_USER_INFO_RESPONSE_ERROR_CODE);
            throw new OAuth2AuthenticationException(oauth2Error, oauth2Error.toString(), ex);
        }
    }
}
