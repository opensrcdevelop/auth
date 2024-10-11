package cn.opensrcdevelop.auth.config;

import cn.opensrcdevelop.auth.filter.TenantContextFilter;
import cn.opensrcdevelop.auth.interceptor.OAuth2ContextInterceptor;
import cn.opensrcdevelop.auth.interceptor.TraceUserInterceptor;
import cn.opensrcdevelop.common.annoation.NoPathPrefix;
import cn.opensrcdevelop.common.filter.RestFilter;
import cn.opensrcdevelop.common.filter.TraceFilter;
import cn.opensrcdevelop.common.interceptor.RestResponseInterceptor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.PathMatchConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebMvcConfig implements WebMvcConfigurer {

    private static final String SWAGGER_PATH = "/swagger-ui/**";

    @Value("${spring.controller.path-prefix}")
    private String pathPrefix;

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        RestResponseInterceptor restResponseInterceptor = new RestResponseInterceptor();
        OAuth2ContextInterceptor oAuth2ContextInterceptor = new OAuth2ContextInterceptor();
        TraceUserInterceptor traceUserInterceptor = new TraceUserInterceptor();

        registry.addInterceptor(restResponseInterceptor)
                .addPathPatterns("/**")
                .excludePathPatterns(SWAGGER_PATH);

        registry.addInterceptor(oAuth2ContextInterceptor)
                .addPathPatterns("/**")
                .excludePathPatterns(SWAGGER_PATH);

        registry.addInterceptor(traceUserInterceptor)
                .addPathPatterns("/**")
                .excludePathPatterns(SWAGGER_PATH);
    }

    @Bean
    public FilterRegistrationBean<RestFilter> traceFilter() {
        TraceFilter traceFilter = new TraceFilter();
        traceFilter.excludePathPatterns(SWAGGER_PATH);

        var filterRegistrationBean = new FilterRegistrationBean<RestFilter>();
        filterRegistrationBean.setFilter(traceFilter);
        filterRegistrationBean.addUrlPatterns("/*");
        filterRegistrationBean.setOrder(Ordered.HIGHEST_PRECEDENCE);
        return filterRegistrationBean;
    }

    @Bean
    public FilterRegistrationBean<RestFilter> tenantContextFilter() {
        TenantContextFilter tenantContextFilter = new TenantContextFilter();
        tenantContextFilter.excludePathPatterns(pathPrefix + "/tenant/check/*");

        var filterRegistrationBean = new FilterRegistrationBean<RestFilter>();
        filterRegistrationBean.setFilter(tenantContextFilter);
        filterRegistrationBean.addUrlPatterns("/*");
        filterRegistrationBean.setOrder(Ordered.HIGHEST_PRECEDENCE + 1);
        return filterRegistrationBean;
    }

    /**
     * 为接口配置统一前缀
     */
    @Override
    public void configurePathMatch(PathMatchConfigurer configurer) {
        configurer.addPathPrefix(pathPrefix, c -> !c.isAnnotationPresent(NoPathPrefix.class) && (c.isAnnotationPresent(RestController.class) || c.isAnnotationPresent(Controller.class)));
    }
}
