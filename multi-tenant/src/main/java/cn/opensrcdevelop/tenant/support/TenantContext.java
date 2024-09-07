package cn.opensrcdevelop.tenant.support;

import com.alibaba.ttl.TransmittableThreadLocal;

public class TenantContext {

    private static final ThreadLocal<String> TENANT_LOCAL = new TransmittableThreadLocal<>();

    private TenantContext() {
    }

    public static void setTenant(String tenant) {
        TENANT_LOCAL.set(tenant);
    }

    public static String getTenant() {
        return TENANT_LOCAL.get();
    }

    public static void remove() {
        TENANT_LOCAL.remove();
    }
}
