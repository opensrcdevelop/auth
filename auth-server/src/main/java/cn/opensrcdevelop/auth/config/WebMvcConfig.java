package cn.opensrcdevelop.auth.config;

import cn.opensrcdevelop.auth.biz.service.client.ClientService;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.auth.filter.TenantContextFilter;
import cn.opensrcdevelop.auth.interceptor.OAuth2ContextInterceptor;
import cn.opensrcdevelop.auth.interceptor.OpenApiInterceptor;
import cn.opensrcdevelop.auth.interceptor.TraceUserInterceptor;
import cn.opensrcdevelop.common.annoation.NoPathPrefix;
import cn.opensrcdevelop.common.config.AuthorizationServerProperties;
import cn.opensrcdevelop.common.filter.ForwardFilter;
import cn.opensrcdevelop.common.filter.RestFilter;
import cn.opensrcdevelop.common.filter.TraceFilter;
import cn.opensrcdevelop.common.interceptor.RestResponseInterceptor;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.http.CacheControl;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.*;

import java.util.concurrent.TimeUnit;

@Configuration
@RequiredArgsConstructor
public class WebMvcConfig implements WebMvcConfigurer {

    private static final String SWAGGER_PATH = "/swagger-ui/**";
    private static final String UI_PATH = "/ui/**";
    private final AuthorizationServerProperties authorizationServerProperties;

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        RestResponseInterceptor restResponseInterceptor = new RestResponseInterceptor();
        OAuth2ContextInterceptor oAuth2ContextInterceptor = new OAuth2ContextInterceptor();
        TraceUserInterceptor traceUserInterceptor = new TraceUserInterceptor();
        OpenApiInterceptor openApiInterceptor = new OpenApiInterceptor(
                SpringContextUtil.getBean(SystemSettingService.class),
                SpringContextUtil.getBean(ClientService.class));

        registry.addInterceptor(restResponseInterceptor)
                .addPathPatterns("/**")
                .excludePathPatterns(SWAGGER_PATH, UI_PATH);

        registry.addInterceptor(oAuth2ContextInterceptor)
                .addPathPatterns("/**")
                .excludePathPatterns(SWAGGER_PATH, UI_PATH);

        registry.addInterceptor(traceUserInterceptor)
                .addPathPatterns("/**")
                .excludePathPatterns(SWAGGER_PATH, UI_PATH);

        registry.addInterceptor(openApiInterceptor)
                .addPathPatterns("/**")
                .excludePathPatterns(SWAGGER_PATH, UI_PATH);
    }

    @Bean
    public FilterRegistrationBean<RestFilter> traceFilter() {
        TraceFilter traceFilter = new TraceFilter();
        traceFilter.excludePathPatterns(SWAGGER_PATH, UI_PATH);

        var filterRegistrationBean = new FilterRegistrationBean<RestFilter>();
        filterRegistrationBean.setFilter(traceFilter);
        filterRegistrationBean.addUrlPatterns("/*");
        filterRegistrationBean.setOrder(Ordered.HIGHEST_PRECEDENCE);
        return filterRegistrationBean;
    }

    @Bean
    public FilterRegistrationBean<RestFilter> tenantContextFilter() {
        TenantContextFilter tenantContextFilter = new TenantContextFilter();
        tenantContextFilter.excludePathPatterns(authorizationServerProperties.getApiPrefix() + "/tenant/check/*",
                UI_PATH);

        var filterRegistrationBean = new FilterRegistrationBean<RestFilter>();
        filterRegistrationBean.setFilter(tenantContextFilter);
        filterRegistrationBean.addUrlPatterns("/*");
        filterRegistrationBean.setOrder(Ordered.HIGHEST_PRECEDENCE + 1);
        return filterRegistrationBean;
    }

    @Bean
    public FilterRegistrationBean<RestFilter> vueRouterForward() {
        // vue页面路由转发至 /ui/index.html
        ForwardFilter forwardFilter = new ForwardFilter("/ui/index.html");
        forwardFilter.excludePathPatterns("/ui/assets/*", "/ui/logo.png", "/ui/favicon.ico");
        var filterRegistrationBean = new FilterRegistrationBean<RestFilter>();
        filterRegistrationBean.setFilter(forwardFilter);
        filterRegistrationBean.addUrlPatterns("/ui/*");
        filterRegistrationBean.setOrder(Ordered.HIGHEST_PRECEDENCE + 2);
        return filterRegistrationBean;
    }

    @Override
    public void configurePathMatch(PathMatchConfigurer configurer) {
        // 为接口配置统一前缀
        configurer.addPathPrefix(authorizationServerProperties.getApiPrefix(),
                c -> !c.isAnnotationPresent(NoPathPrefix.class)
                        && (c.isAnnotationPresent(RestController.class) || c.isAnnotationPresent(Controller.class)));
    }

    @Override
    public void addViewControllers(ViewControllerRegistry registry) {
        // 首页重定向
        registry.addRedirectViewController("/", "/ui");
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/ui/assets/*.js", "/ui/assets/*.css")
                .setCacheControl(CacheControl.maxAge(365, TimeUnit.DAYS).cachePublic())
                .addResourceLocations("classpath:/ui/assets/");

        registry.addResourceHandler("/ui/**")
                .addResourceLocations("classpath:/ui/");
    }
}
