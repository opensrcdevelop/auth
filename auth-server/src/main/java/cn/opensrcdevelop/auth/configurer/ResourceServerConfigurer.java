package cn.opensrcdevelop.auth.configurer;

import cn.opensrcdevelop.auth.authentication.email.EmailCodeAuthenticationFilter;
import cn.opensrcdevelop.auth.authentication.email.EmailCodeAuthenticationProvider;
import cn.opensrcdevelop.auth.component.AuthorizationServerProperties;
import cn.opensrcdevelop.auth.filter.ChangePwdCheckFilter;
import cn.opensrcdevelop.auth.filter.TotpValidFilter;
import cn.opensrcdevelop.auth.handler.*;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.service.UserService;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.oauth2.server.resource.introspection.OpaqueTokenIntrospector;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.util.UrlUtils;
import org.springframework.web.filter.CorsFilter;

/**
 * 资源服务器配置
 */
@RequiredArgsConstructor
public class ResourceServerConfigurer extends AbstractHttpConfigurer<ResourceServerConfigurer, HttpSecurity> {

    private final CorsFilter corsFilter;
    private final TotpValidFilter totpValidFilter;
    private final ChangePwdCheckFilter changePwdCheckFilter;
    private final AuthorizationServerProperties authorizationServerProperties;
    private final OpaqueTokenIntrospector tokenIntrospector;

    @Override
    public void init(HttpSecurity http) throws Exception {
        // 表单 login 配置
        http
                .authorizeHttpRequests(x -> {
                    x.requestMatchers(authorizationServerProperties.getIgnoreAuthenticationUriList().toArray(new String[0])).permitAll();
                    x.anyRequest().authenticated();
                })
                .formLogin(x -> {
                    x.loginPage("/login");
                    if (UrlUtils.isAbsoluteUrl(authorizationServerProperties.getLoginPageUrl())) {
                        x.successHandler(new LoginSuccessHandler());
                        x.failureHandler(new LoginFailureHandler());
                    }
                });

        // 资源服务器配置
        http.oauth2ResourceServer(x -> {
            x.accessDeniedHandler(new ResourceAccessDeniedHandler());
            x.authenticationEntryPoint(new ResourceAuthenticationExceptionHandler());
            // 验证 Token
            if (Boolean.TRUE.equals(authorizationServerProperties.getIntrospectToken())) {
                x.opaqueToken(t -> t.introspector(tokenIntrospector));
            } else {
                x.opaqueToken(Customizer.withDefaults());
            }
        });

        // 登出处理
        http.logout(x -> {
            x.logoutSuccessHandler(new AuthLogoutSuccessHandler());
            x.deleteCookies(AuthConstants.COOKIE_SESSION);
            x.invalidateHttpSession(true);
        });

        // 禁用 cors 和 csrf
        http.csrf(AbstractHttpConfigurer::disable);
        http.cors(AbstractHttpConfigurer::disable);
    }

    @Override
    public void configure(HttpSecurity http) throws Exception {
        // 添加 cors 过滤器
        http.addFilter(corsFilter);
        // 添加变更密码检查过滤器
        http.addFilterBefore(changePwdCheckFilter, UsernamePasswordAuthenticationFilter.class);
        // 添加 Totp 校验过滤器
        http.addFilterAfter(totpValidFilter, UsernamePasswordAuthenticationFilter.class);

        // 添加邮箱验证码登录
        AuthenticationManager authenticationManager = http.getSharedObject(AuthenticationManager.class);
        EmailCodeAuthenticationFilter emailCodeAuthenticationFilter = new EmailCodeAuthenticationFilter(authenticationManager);
        http.addFilterBefore(emailCodeAuthenticationFilter, UsernamePasswordAuthenticationFilter.class);
        http.authenticationProvider(new EmailCodeAuthenticationProvider(SpringContextUtil.getBean(UserService.class)));
    }
}
