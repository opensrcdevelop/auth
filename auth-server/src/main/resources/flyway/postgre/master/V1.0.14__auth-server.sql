/**
 * 变更：
 *      1. 修改表【t_client】
 *
 */

UPDATE "t_client" 
SET 
    "client_authentication_methods" = 'client_secret_post,none,client_secret_basic',
    "authorization_grant_types" = 'refresh_token,authorization_code',
    "post_logout_redirect_uris" = '',
    "scopes" = '',
    "client_secret" = NULL,
    "client_settings" = '{"@class":"java.util.HashMap","settings.client.require-proof-key":true,"settings.client.require-authorization-consent":false}',
    "token_settings" = '{"@class":"java.util.HashMap","settings.token.reuse-refresh-tokens":true,"settings.token.id-token-signature-algorithm":["org.springframework.security.oauth2.jose.jws.SignatureAlgorithm","RS256"],"settings.token.access-token-time-to-live":["java.time.Duration",86400.000000000],"settings.token.access-token-format":{"@class":"org.springframework.security.oauth2.server.authorization.settings.OAuth2TokenFormat","value":"self-contained"},"settings.token.refresh-token-time-to-live":["java.time.Duration",604800.000000000],"settings.token.authorization-code-time-to-live":["java.time.Duration",300.000000000],"settings.token.device-code-time-to-live":["java.time.Duration",300.000000000]}'
WHERE "client_id" = '52cb8d26-a352-4e5c-99a7-d52b8afff3b1';