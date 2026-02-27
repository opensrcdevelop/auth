package cn.opensrcdevelop.auth.support;

import cn.opensrcdevelop.auth.biz.constants.SystemSettingConstants;
import cn.opensrcdevelop.auth.biz.entity.client.Client;
import cn.opensrcdevelop.auth.biz.service.client.ClientService;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.tenant.component.MultiTenantProperties;
import cn.opensrcdevelop.tenant.entity.Tenant;
import cn.opensrcdevelop.tenant.service.TenantService;
import cn.opensrcdevelop.tenant.support.TenantContext;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.Ordered;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class AuthServerApplicationRunner implements ApplicationRunner, Ordered {

    private final TenantService tenantService;
    private final ClientService clientService;
    private final SystemSettingService systemSettingService;

    /**
     * 执行应用程序启动初始化任务
     */
    @Override
    public void run(ApplicationArguments args) throws Exception {
        executeWithTenantContext(() -> {
            // 存储控制台客户端 ID
            Client client = clientService
                    .getOne(Wrappers.<Client>lambdaQuery().eq(Client::getClientName,
                            CommonConstants.CONSOLE_CLIENT_NAME));
            if (Objects.nonNull(client)) {
                String clientId = client.getClientId();
                systemSettingService.saveSystemSetting(SystemSettingConstants.CONSOLE_CLIENT_ID, clientId);
            }
        });
    }

    @Override
    public int getOrder() {
        return Ordered.LOWEST_PRECEDENCE;
    }

    private void executeWithTenantContext(Runnable task) {
        TenantHelper.clearTenantDsContext();
        // 1. 获取全部租户
        var tenants = tenantService.list(Wrappers.<Tenant>lambdaQuery().eq(Tenant::getEnabled, Boolean.TRUE));
        // 1.1 添加默认租户
        Tenant defaultTenant = new Tenant();
        defaultTenant.setTenantCode(SpringContextUtil.getBean(MultiTenantProperties.class).getDefaultTenant());
        tenants.addFirst(defaultTenant);

        for (var tenant : tenants) {
            // 2. 分别为租户执行任务
            // 2.1 设置线程上下文
            String tenantCode = tenant.getTenantCode();
            TenantContext tenantContext = new TenantContext();
            tenantContext.setTenantCode(tenantCode);
            TenantContextHolder.setTenantContext(tenantContext);

            // 2.2 切换租户数据源
            TenantHelper.switchTenantDs(tenantCode);

            try {
                // 2.3 执行任务
                task.run();
            } finally {
                TenantContextHolder.removeTenantContext();
            }
        }
    }
}
