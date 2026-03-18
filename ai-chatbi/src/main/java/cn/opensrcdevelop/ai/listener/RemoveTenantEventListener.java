package cn.opensrcdevelop.ai.listener;

import cn.opensrcdevelop.ai.service.SampleSqlVectorStoreService;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.tenant.entity.Tenant;
import cn.opensrcdevelop.tenant.event.RemoveTenantEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class RemoveTenantEventListener implements ApplicationListener<RemoveTenantEvent> {

    private final SampleSqlVectorStoreService sampleSqlVectorStoreService;

    @Override
    @Async(ExecutorConstants.EXECUTOR_IO_DENSE)
    public void onApplicationEvent(RemoveTenantEvent event) {
        Tenant tenant = (Tenant) event.getSource();
        sampleSqlVectorStoreService.removeCollection(tenant.getTenantCode());
    }
}
