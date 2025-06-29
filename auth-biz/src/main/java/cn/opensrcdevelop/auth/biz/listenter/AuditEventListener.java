package cn.opensrcdevelop.auth.biz.listenter;

import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import cn.opensrcdevelop.auth.audit.entity.ObjChangeLog;
import cn.opensrcdevelop.auth.audit.event.AuditEvent;
import cn.opensrcdevelop.auth.biz.service.audit.AuditLogService;
import cn.opensrcdevelop.auth.biz.service.audit.ObjChangeLogService;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;

@Component
@RequiredArgsConstructor
@Transactional
public class AuditEventListener implements ApplicationListener<AuditEvent> {

    private final AuditLogService auditLogService;
    private final ObjChangeLogService objChangeLogService;

    @Override
    @Async(ExecutorConstants.EXECUTOR_IO_DENSE)
    public void onApplicationEvent(AuditEvent event) {
        // 1. 保存审计日志
        AuditLog auditLog = event.getAuditLog();
        if (Objects.isNull(auditLog)) {
            return;
        }
        auditLogService.save(auditLog);

        if (CollectionUtils.isNotEmpty(event.getObjChangeLogs())) {
            // 2. 保存对象变更日志
            for (ObjChangeLog objChangeLog : event.getObjChangeLogs()) {
                objChangeLog.setAuditId(auditLog.getAuditId());
            }
            objChangeLogService.saveBatch(event.getObjChangeLogs());
        }
    }
}
