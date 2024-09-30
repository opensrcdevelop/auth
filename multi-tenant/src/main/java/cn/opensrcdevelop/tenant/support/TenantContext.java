package cn.opensrcdevelop.tenant.support;

import lombok.Data;

@Data
public class TenantContext {

    /** 租户标识 */
    private String tenantCode;

    /** 租户名称 */
    private String tenantName;

    /** 是否为默认租户 */
    private boolean defaultTenant;
}
