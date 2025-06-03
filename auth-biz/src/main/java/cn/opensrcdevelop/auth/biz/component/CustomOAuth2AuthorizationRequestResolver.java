package cn.opensrcdevelop.auth.biz.component;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.dto.identity.RequestConfigRequestDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceProvider;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.auth.biz.util.HttpExpressionUtil;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateExceptionHandler;
import jakarta.servlet.http.HttpServletRequest;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.client.web.DefaultOAuth2AuthorizationRequestResolver;
import org.springframework.security.oauth2.client.web.OAuth2AuthorizationRequestResolver;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationRequest;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.stereotype.Component;

import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

@Component
public class CustomOAuth2AuthorizationRequestResolver implements OAuth2AuthorizationRequestResolver {

    private static final String REGISTRATION_ID_URI_VARIABLE_NAME = "registrationId";

    private final DefaultOAuth2AuthorizationRequestResolver delegate;
    private final IdentitySourceRegistrationService identitySourceRegistrationService;
    private final AntPathRequestMatcher authorizationRequestMatcher = new AntPathRequestMatcher(AuthConstants.FEDERATION_LOGIN_URI + "/{" + REGISTRATION_ID_URI_VARIABLE_NAME + "}");

    public CustomOAuth2AuthorizationRequestResolver(ClientRegistrationRepository clientRegistrationRepository, IdentitySourceRegistrationService identitySourceRegistrationService) {
        delegate = new DefaultOAuth2AuthorizationRequestResolver(clientRegistrationRepository, AuthConstants.FEDERATION_LOGIN_URI);
        this.identitySourceRegistrationService = identitySourceRegistrationService;
    }

    @Override
    public OAuth2AuthorizationRequest resolve(HttpServletRequest request) {
        String registrationId = resolveRegistrationId(request);
        if (Objects.isNull(registrationId)) {
            return delegate.resolve(request);
        }
        return resolve(request, resolveRegistrationId(request));
    }

    @Override
    public OAuth2AuthorizationRequest resolve(HttpServletRequest request, String clientRegistrationId) {
        // 1. 获取身份源提供商
        IdentitySourceRegistration identitySourceRegistration = identitySourceRegistrationService.getByCode(clientRegistrationId);
        if (Objects.isNull(identitySourceRegistration)) {
            throw new IllegalArgumentException("无效的身份源标识：" + clientRegistrationId);
        }
        IdentitySourceProvider identitySourceProvider = identitySourceRegistration.getIdentitySourceProvider();
        delegate.setAuthorizationRequestCustomizer(authorizationRequest -> {
            // 2. 是否启用自定义授权请求参数
            if (Boolean.TRUE.equals(identitySourceProvider.getEnableCustomAuthzReq())) {
                authorizationRequest.parameters(parameters -> {
                    Map<String, Object> customAuthzReqContext = new HashMap<>(parameters);
                    if (StringUtils.isNotBlank(identitySourceRegistration.getAdditionalParams())) {
                        customAuthzReqContext.putAll(CommonUtil.deserializeObject(identitySourceRegistration.getAdditionalParams(), new TypeReference<Map<String, Object>>() {}));
                    }
                    parameters.clear();

                    // 2.1 填充参数
                    String requestCfgStr = fillRequestCfg(identitySourceProvider.getAuthzReqCfg(), customAuthzReqContext);

                    // 2.2 反序列化
                    RequestConfigRequestDto requestCfg = CommonUtil.deserializeObject(requestCfgStr, RequestConfigRequestDto.class);

                    // 2.3 执行 SpEL 表达式
                    HttpExpressionUtil.parseSpELMap(requestCfg.getParams());

                    // 2.4 添加 query 参数
                    parameters.putAll(requestCfg.getParams());
                });
            }
        });
        return delegate.resolve(request, clientRegistrationId);
    }

    private String fillRequestCfg(String requestCfg, Map<String, Object> requestCfgValueContext) {
        // 1. 填充模版参数
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

    private String resolveRegistrationId(HttpServletRequest request) {
        if (this.authorizationRequestMatcher.matches(request)) {
            return this.authorizationRequestMatcher.matcher(request)
                    .getVariables()
                    .get(REGISTRATION_ID_URI_VARIABLE_NAME);
        }
        return null;
    }
}
