package cn.opensrcdevelop.tenant.support;

import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.tenant.component.MultiTenantProperties;
import com.alibaba.ttl.TransmittableThreadLocal;
import jakarta.servlet.http.HttpServletRequest;
import java.net.URI;
import java.net.URL;
import org.apache.commons.lang3.StringUtils;

public class TenantContextHolder {

    private TenantContextHolder() {
    }

    private static final ThreadLocal<TenantContext> TENANT_LOCAL = new TransmittableThreadLocal<>();

    public static void setTenantContext(TenantContext tenant) {
        TENANT_LOCAL.set(tenant);
    }

    public static void setTenantContext(HttpServletRequest request) {
        try {
            // 1. 根据域名获取租户标识
            URL baseUrl = URI.create(SpringContextUtil.getProperty(CommonConstants.PROP_DEFAULT_ISSUER)).toURL();
            URL requestUrl = URI.create(request.getRequestURL().toString()).toURL();
            // 1.1 默认租户
            if (StringUtils.equals(baseUrl.getHost(), requestUrl.getHost())) {
                MultiTenantProperties multiTenantProperties = SpringContextUtil.getBean(MultiTenantProperties.class);
                TenantContext tenantContext = new TenantContext();
                tenantContext.setTenantCode(multiTenantProperties.getDefaultTenant());
                tenantContext.setDefaultTenant(true);
                setTenantContext(tenantContext);
                return;
            }
            String tenantCode = requestUrl.getHost().split("\\.")[0];
            // 1.2 非默认租户
            TenantContext tenantContext = new TenantContext();
            tenantContext.setTenantCode(tenantCode);
            tenantContext.setDefaultTenant(false);
            setTenantContext(tenantContext);

        } catch (Exception ex) {
            throw new ServerException(ex);
        }
    }

    public static TenantContext getTenantContext() {
        return TENANT_LOCAL.get();
    }

    public static void removeTenantContext() {
        TENANT_LOCAL.remove();
    }
}
