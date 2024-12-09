package cn.opensrcdevelop.auth.component;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;

@ConfigurationProperties(prefix = "auth.server")
@Data
public class AuthorizationServerProperties {

    private String defaultIssuer;

    private String defaultConsoleUrl;

    private String consentPageUrl = "/oauth2/consent";

    private String loginPageUrl = "/login";

    private String consoleRedirectPath;

    /** 无需认证的 URI 集合 */
    private List<String> ignoreAuthenticationUriList;

    /** 是否开启多次登录失败后禁用账户 */
    private Boolean enableLockAccount = false;

    /** 最大登录失败次数 */
    private Integer maxLoginFailedCnt = 3;

    /** 验证 Token */
    private Boolean introspectToken;

    /** 开启单点登录 */
    private Boolean enableSso = false;

    /** 最大登录日志数 */
    private Integer maxLoginLogNum = -1;
}
