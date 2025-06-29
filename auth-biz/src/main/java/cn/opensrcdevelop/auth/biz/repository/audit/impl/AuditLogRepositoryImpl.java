package cn.opensrcdevelop.auth.biz.repository.audit.impl;

import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import cn.opensrcdevelop.auth.biz.mapper.audit.AuditLogMapper;
import cn.opensrcdevelop.auth.biz.repository.audit.AuditLogRepository;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;

@Repository
@RequiredArgsConstructor
public class AuditLogRepositoryImpl implements AuditLogRepository {

    public final AuditLogMapper auditLogMapper;


    /**
     * 检索用户操作日志
     *
     * @param page 分页对象
     * @param keyword 用户ID / 用户名 / IP 检索关键字
     * @param operationType 操作类型
     * @param startTime 开始时间
     * @param endTime 结束时间
     */
    @Override
    public void searchUserOperationLog(IPage<AuditLog> page, String keyword, Integer operationType, LocalDateTime startTime, LocalDateTime endTime) {
        auditLogMapper.searchUserOperationLog(page, keyword, operationType, startTime, endTime);
    }

    /**
     * 检索系统操作日志
     *
     * @param page 分页对象
     * @param keyword 用户ID / 用户名 / IP 检索关键字
     * @param operationType 操作类型
     * @param resourceId 资源ID
     * @param startTime 开始时间
     * @param endTime 结束时间
     */
    @Override
    public void searchSysOperationLog(IPage<AuditLog> page, String keyword, Integer operationType, String resourceId, LocalDateTime startTime, LocalDateTime endTime) {
        auditLogMapper.searchSysOperationLog(page, keyword, operationType, resourceId, startTime, endTime);
    }
}
