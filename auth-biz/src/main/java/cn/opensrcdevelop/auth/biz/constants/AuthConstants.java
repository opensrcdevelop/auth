package cn.opensrcdevelop.auth.biz.constants;

/**
 * 认证常量类
 */
public class AuthConstants {

    private AuthConstants() {}

    public static final String SECURITY_CONTEXT_REDIS_PREFIX = "security_context:";
    public static final String JWK_REDIS_KEY = "auth_server_jwk:";
    public static final String GRANT_TYPE_PASSWORD = "password";
    public static final String COOKIE_SESSION = "SESSION";
    public static final String TOTP_VALID_CONTEXT = "TOTP_VALID_CONTEXT";
    public static final String VERIFICATION_CODE = "VERIFICATION_CODE";
    public static final String SESSION_CHANGED_PWD = "SESSION_CHANGED_PWD";
    public static final String OIDC_SCOPE_ROLES = "roles";
    public static final String CONSOLE_ACCESS = "consoleAccess";
}