package cn.opensrcdevelop.auth.filter;

import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.filter.RestFilter;
import cn.opensrcdevelop.common.filter.TraceFilter;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import cn.opensrcdevelop.tenant.component.MultiTenantProperties;
import cn.opensrcdevelop.tenant.constants.MessageConstants;
import cn.opensrcdevelop.tenant.support.TenantContext;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.http.HttpStatus;

@Slf4j
public class TenantContextFilter extends RestFilter {

    @Override
    protected void doSubFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {
        try {
            MultiTenantProperties multiTenantProperties = SpringContextUtil.getBean(MultiTenantProperties.class);

            // 1. 检查请求头中是否包含租户标识
            String tenantCode = request.getHeader(CommonConstants.REQ_HEADER_X_TENANT_CODE);
            if (StringUtils.isBlank(tenantCode)) {
                // 2. 根据域名获取租户标识
                URL baseUrl = URI.create(SpringContextUtil.getProperty(CommonConstants.PROP_DEFAULT_ISSUER)).toURL();
                URL requestUrl = URI.create(request.getRequestURL().toString()).toURL();
                if (StringUtils.equals(baseUrl.getHost(), requestUrl.getHost())) {
                    tenantCode = multiTenantProperties.getDefaultTenant();
                } else {
                    tenantCode = requestUrl.getHost().split("\\.")[0];
                }
            }

            // 3.1 默认租户
            if (multiTenantProperties.getDefaultTenant().equals(tenantCode)) {
                // 3.1.1 设置租户线程上下文
                TenantContext tenantContext = new TenantContext();
                tenantContext.setTenantCode(tenantCode);
                tenantContext.setDefaultTenant(true);
                setTenantContext(tenantContext);

                filterChain.doFilter(request, response);
                return;
            }

            // 4. 检查租户是否存在
            var existsRes = TenantHelper.tenantExists(tenantCode);
            if (Boolean.TRUE.equals(existsRes._1)) {
                // 4.1 设置租户线程上下文
                TenantContext tenantContext = new TenantContext();
                tenantContext.setTenantCode(existsRes._2.getTenantCode());
                tenantContext.setTenantName(existsRes._2.getTenantName());
                tenantContext.setDefaultTenant(false);
                setTenantContext(tenantContext);

                filterChain.doFilter(request, response);
                return;
            }
            WebUtil.sendJsonResponse(response, R.optFail(MessageConstants.TENANT_MSG_1000, tenantCode),
                    HttpStatus.NOT_FOUND);
        } finally {
            // 5. 清空租户线程上下文和 session 属性
            TenantHelper.clearTenantContext();
            TenantHelper.clearTenantDsContext();
        }
    }

    private void setTenantContext(TenantContext tenantContext) {
        String tenantCode = tenantContext.getTenantCode();
        MDC.put(CommonConstants.MDC_TENANT_CODE, tenantCode);
        TraceFilter.TTL_MDC.get().put(CommonConstants.MDC_TENANT_CODE, tenantCode);
        // 切换租户数据源
        TenantHelper.switchTenantDs(tenantCode);
        // 设置租户上下文
        TenantContextHolder.setTenantContext(tenantContext);
    }
}
