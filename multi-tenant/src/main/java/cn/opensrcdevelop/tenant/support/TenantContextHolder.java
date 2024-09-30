package cn.opensrcdevelop.tenant.support;

import com.alibaba.ttl.TransmittableThreadLocal;

public class TenantContextHolder {

    private TenantContextHolder() {}

    private static final ThreadLocal<TenantContext> TENANT_LOCAL = new TransmittableThreadLocal<>();

    public static void setTenantContext(TenantContext tenant) {
        TENANT_LOCAL.set(tenant);
    }

    public static TenantContext getTenantContext() {
        return TENANT_LOCAL.get();
    }

    public static void removeTenantContext() {
        TENANT_LOCAL.remove();
    }
}
