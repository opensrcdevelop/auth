package cn.opensrcdevelop.auth.config;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeIn;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityScheme;

@OpenAPIDefinition(
        info = @Info(
                title = "Auth Gateway RESTful API",
                description = "认证网关 API 文档",
                version = "v1.0.0",
                license = @io.swagger.v3.oas.annotations.info.License(
                        name = "MIT",
                        url = "https://opensource.org/licenses/MIT"
                )
        ),
        security = {
                @SecurityRequirement(name = "OIDC Flow",  scopes = {"openid", "email_phone"}),
                @SecurityRequirement(name = "Bearer access_token"),
        }
)
@SecurityScheme(
        name = "OIDC Flow",
        type = SecuritySchemeType.OPENIDCONNECT,
        openIdConnectUrl = "${springdoc.swagger-ui.oauth.oidc-url}",
        description = "OpenIdConnect认证流程，<br/>由OIDC发现端点自动识别支持的授权流程，<br/>根据需要选择下方的Scopes。"
)
@SecurityScheme(
        name = "Bearer access_token",
        type = SecuritySchemeType.HTTP,
        in = SecuritySchemeIn.HEADER,
        scheme = "bearer",
        description = "直接将有效的 access_token 填入下方，后续该 access_token 将作为 Bearer access_token"
)
public class SwaggerConfig {
}
