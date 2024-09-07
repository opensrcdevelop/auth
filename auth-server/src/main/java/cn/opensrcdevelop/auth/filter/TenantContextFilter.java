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
import cn.opensrcdevelop.tenant.support.TenantHelper;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.http.HttpStatus;

import java.io.IOException;
import java.net.URL;

@Slf4j
public class TenantContextFilter extends RestFilter {

    private static final String PROP_DEFAULT_ISSUER = "auth.server.default-issuer";

    @Override
    protected void doSubFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        try {
            // 1. 根据域名获取租户标识
            URL baseUrl = new URL(SpringContextUtil.getProperty(PROP_DEFAULT_ISSUER));
            URL requestUrl = new URL(request.getRequestURL().toString());
            // 1.1 默认租户
            if (StringUtils.equals(baseUrl.getHost(), requestUrl.getHost())) {
                // 1.1.1 设置租户线程上下文
                MultiTenantProperties multiTenantProperties = SpringContextUtil.getBean(MultiTenantProperties.class);
                setTenantContext(multiTenantProperties.getDefaultTenant());
                filterChain.doFilter(request, response);
                return;
            }
            String tenantCode = requestUrl.getHost().split("\\.")[0];

            // 2. 检查租户是否存在
            if (TenantHelper.tenantExists(tenantCode)) {
                // 2.1 设置租户线程上下文
                setTenantContext(tenantCode);
                filterChain.doFilter(request, response);
                return;
            }
            WebUtil.sendJsonResponse(response, R.optFail(MessageConstants.TENANT_MSG_1000, tenantCode), HttpStatus.NOT_FOUND);
        } finally {
            // 3. 清空租户线程上下文
            TenantHelper.clearTenantContext();
            TenantHelper.clearTenantDsContext();
        }
    }

    private void setTenantContext(String tenantCode) {
        MDC.put(CommonConstants.MDC_TENANT_CODE, tenantCode);
        TraceFilter.TTL_MDC.get().put(CommonConstants.MDC_TENANT_CODE, tenantCode);
        // 切换租户数据源
        TenantHelper.switchTenantDs(tenantCode);
        // 设置租户上下文
        TenantContext.setTenant(tenantCode);
    }
}
