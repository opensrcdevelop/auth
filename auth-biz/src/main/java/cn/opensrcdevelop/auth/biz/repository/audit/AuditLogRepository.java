package cn.opensrcdevelop.auth.biz.repository.audit;

import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.time.LocalDateTime;

public interface AuditLogRepository {

    void searchUserOperationLog(IPage<AuditLog> page, String keyword, Integer operationType, LocalDateTime startTime, LocalDateTime endTime);

    void searchSysOperationLog(IPage<AuditLog> page, String keyword, Integer operationType, String resourceId, LocalDateTime startTime, LocalDateTime endTime);
}
