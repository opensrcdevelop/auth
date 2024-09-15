package cn.opensrcdevelop.auth.config;

import cn.opensrcdevelop.auth.component.AuthorizationServerProperties;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.session.data.redis.config.annotation.web.http.EnableRedisHttpSession;
import org.springframework.session.web.http.CookieSerializer;
import org.springframework.session.web.http.DefaultCookieSerializer;

@Configuration
@EnableRedisHttpSession
@RequiredArgsConstructor
public class SessionConfig {

    private final AuthorizationServerProperties authorizationServerProperties;

    @Bean
    public CookieSerializer cookieSerializer() {
        DefaultCookieSerializer defaultCookieSerializer = new DefaultCookieSerializer();
        // 不使用 base64 编码
        defaultCookieSerializer.setUseBase64Encoding(false);
        // 开启单点登录
        if (Boolean.TRUE.equals(authorizationServerProperties.getEnableSso())) {
            defaultCookieSerializer.setSameSite("Lax");
        } else {
            defaultCookieSerializer.setSameSite("Strict");
        }
        return defaultCookieSerializer;
    }
}
