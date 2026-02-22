package cn.opensrcdevelop.auth.biz.constants;

/**
 * 认证常量类
 */
public class AuthConstants {

    private AuthConstants() {
    }

    public static final String SECURITY_CONTEXT_REDIS_PREFIX = "security_context:";
    public static final String JWK_REDIS_KEY = "auth_server_jwk:";
    public static final String GRANT_TYPE_PASSWORD = "password";
    public static final String COOKIE_SESSION = "SESSION";
    public static final String MFA_VALID_CONTEXT = "MFA_VALID_CONTEXT";
    public static final String WEB_AUTHN_REGISTER_CHALLENGE = "WEB_AUTHN_REGISTER_CHALLENGE";
    public static final String WEB_AUTHN_AUTHENTICATE_CHALLENGE = "WEB_AUTHN_AUTHENTICATE_CHALLENGE";
    public static final String VERIFICATION_CODE = "VERIFICATION_CODE";
    public static final String SESSION_CHANGED_PWD = "SESSION_CHANGED_PWD";
    public static final String OIDC_SCOPE_ROLES = "roles";
    public static final String CONSOLE_ACCESS = "consoleAccess";
    public static final String SESSION_LOGIN_ID = "LOGIN_ID";
    public static final String REMEMBER_ME = "rememberMe";
    public static final String LOGIN_URL = "/login";
    public static final String LOGOUT_URL = "/logout";
    public static final String EMAIL_LOGIN_URL = "/login/email";
    public static final String PASSKEY_LOGIN_URL = "/login/passkey";
    public static final String FEDERATION_LOGIN_URI = "/login/federation";
    public static final String FEDERATION_LOGIN_REDIRECTION_URI = "/login/federation/callback/*";
    public static final String FEDERATION_LOGIN_REDIRECT_URI_FORMAT = "{baseUrl}%s/login/federation/callback/{registrationId}";
    public static final String SESSION_BIND_REQ_USER_ID = "SESSION_BIND_REQ_USER_ID";
    public static final String THIRD_ACCOUNT_ALREADY_EXISTS_ERROR_CODE = "thirdAccount_already_exists";
    public static final String USER_LOCKED_ERROR_CODE = "user_locked";
    public static final String MFA_METHOD_TOTP = "TOTP";
    public static final String MFA_METHOD_WEBAUTHN = "WEBAUTHN";
}
