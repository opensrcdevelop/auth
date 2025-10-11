package cn.opensrcdevelop.auth.biz.service.audit;

import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import cn.opensrcdevelop.auth.biz.dto.audit.AuditLogResponseDto;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.time.LocalDateTime;

public interface AuditLogService extends IService<AuditLog> {

    PageData<AuditLogResponseDto> getUserOperationLogs(int page, int size, String keyword, Integer type, LocalDateTime start, LocalDateTime end);

    PageData<AuditLogResponseDto> getSysOperationLogs(int page, int size, String keyword, Integer type, String resourceId, LocalDateTime start, LocalDateTime end);
}
