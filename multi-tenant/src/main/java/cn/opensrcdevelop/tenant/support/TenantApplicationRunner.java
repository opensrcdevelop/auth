package cn.opensrcdevelop.tenant.support;

import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.tenant.entity.Tenant;
import cn.opensrcdevelop.tenant.service.TenantService;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Component
@RequiredArgsConstructor
@Slf4j
public class TenantApplicationRunner implements ApplicationRunner {

    private final TenantService tenantService;

    private static final String FLYWAY_PLACEHOLDER_CONSOLE_REDIRECT_URL = "consoleRedirectUrl";
    private static final String PROP_CONSOLE_REDIRECT_PATH = "auth.server.console-redirect-path";

    @Override
    public void run(ApplicationArguments args) throws Exception {
        log.info("开始执行全部租户数据库迁移脚本");
        // 1. 获取全部租户
        var tenants = tenantService.list(Wrappers.<Tenant>lambdaQuery().eq(Tenant::getEnabled, Boolean.TRUE));

        // 2. 执行租户数据库迁移脚本
        var tasks = CommonUtil.stream(tenants).map(tenant ->
             CompletableFuture.runAsync(() -> {
                String tenantCode = tenant.getTenantCode();
                // 2.1 创建租户数据源
                var ds = TenantHelper.createTenantDataSource(tenantCode);
                // 2.2 执行
                TenantHelper.executeTenantDbMigrations(ds, Map.of(FLYWAY_PLACEHOLDER_CONSOLE_REDIRECT_URL,
                        TenantHelper.getTenantConsoleUrl(tenantCode) + SpringContextUtil.getProperty(PROP_CONSOLE_REDIRECT_PATH)));
            }, SpringContextUtil.getBean(ExecutorConstants.EXECUTOR_IO_DENSE))
        ).toList();
        CompletableFuture.allOf(tasks.toArray(new CompletableFuture[0])).join();
        log.info("结束执行全部租户数据库迁移脚本");
    }
}
