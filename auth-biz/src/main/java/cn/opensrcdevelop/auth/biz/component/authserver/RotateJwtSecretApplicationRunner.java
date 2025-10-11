package cn.opensrcdevelop.auth.biz.component.authserver;

import cn.opensrcdevelop.auth.biz.component.ScheduledTaskService;
import cn.opensrcdevelop.auth.biz.dto.system.jwt.JwtSecretInfoDto;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.tenant.component.MultiTenantProperties;
import cn.opensrcdevelop.tenant.entity.Tenant;
import cn.opensrcdevelop.tenant.service.TenantService;
import cn.opensrcdevelop.tenant.support.TenantContext;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.Ordered;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
@RequiredArgsConstructor
@Slf4j
public class RotateJwtSecretApplicationRunner implements ApplicationRunner, Ordered {

    public static final String ROTATE_JWT_SECRET_TASK_NAME_PREFIX = "rotate_jwt_secret_task_";

    private final SystemSettingService systemSettingService;
    private final ScheduledTaskService scheduledTaskService;
    private final TenantService tenantService;

    @Override
    public void run(ApplicationArguments args) throws Exception {
        TenantHelper.clearTenantDsContext();
        // 1. 获取全部租户
        var tenants = tenantService.list(Wrappers.<Tenant>lambdaQuery().eq(Tenant::getEnabled, Boolean.TRUE));
        // 1.1 添加默认租户
        Tenant defaultTenant = new Tenant();
        defaultTenant.setTenantCode(SpringContextUtil.getBean(MultiTenantProperties.class).getDefaultTenant());
        tenants.addFirst(defaultTenant);

        for (var tenant : tenants) {
            try {
                // 2. 分别为租户添加轮换密钥任务
                // 2.1 设置线程上下文
                String tenantCode = tenant.getTenantCode();
                TenantContext tenantContext = new TenantContext();
                tenantContext.setTenantCode(tenantCode);
                TenantContextHolder.setTenantContext(tenantContext);

                // 2.2 切换租户数据源
                TenantHelper.switchTenantDs(tenantCode);

                // 2.3 获取 JWT 密钥信息
                JwtSecretInfoDto jwtSecretInfoDto = systemSettingService.getJwtSecretInfo();

                if (Objects.nonNull(jwtSecretInfoDto) && Objects.nonNull(jwtSecretInfoDto.getExpireTime())) {
                    // 2.4 添加轮换密钥任务
                    scheduledTaskService.addTaskAtFixedTime(ROTATE_JWT_SECRET_TASK_NAME_PREFIX + tenantCode, () -> systemSettingService.rotateJwtSecret(tenantCode), jwtSecretInfoDto.getExpireTime());
                } else {
                    log.info("租户[{}]无轮换密钥任务。", tenantCode);
                }
            } finally {
                TenantContextHolder.removeTenantContext();
            }
        }
    }

    @Override
    public int getOrder() {
        return Ordered.LOWEST_PRECEDENCE;
    }
}
