package cn.opensrcdevelop.auth.biz.service.audit.impl;

import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import cn.opensrcdevelop.auth.biz.dto.audit.AuditLogResponseDto;
import cn.opensrcdevelop.auth.biz.mapper.audit.AuditLogMapper;
import cn.opensrcdevelop.auth.biz.repository.audit.AuditLogRepository;
import cn.opensrcdevelop.auth.biz.service.audit.AuditLogService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.time.LocalDateTime;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AuditLogServiceImpl extends ServiceImpl<AuditLogMapper, AuditLog> implements AuditLogService {

    private final AuditLogRepository auditLogRepository;

    /**
     * 获取用户操作日志
     *
     * @param page
     *            页数
     * @param size
     *            条数
     * @param keyword
     *            用户ID / 用户名 / IP 检索关键字
     * @param type
     *            操作类型
     * @param start
     *            开始时间
     * @param end
     *            结束时间
     * @return 用户操作日志
     */
    @Override
    public PageData<AuditLogResponseDto> getUserOperationLogs(int page, int size, String keyword, Integer type,
            LocalDateTime start, LocalDateTime end) {
        // 1. 数据库操作
        Page<AuditLog> pageRequest = new Page<>(page, size);
        auditLogRepository.searchUserOperationLog(pageRequest, keyword, type, start, end);

        // 2. 属性设置
        PageData<AuditLogResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var records = CommonUtil.stream(pageRequest.getRecords()).map(auditLog -> AuditLogResponseDto.builder()
                .id(auditLog.getAuditId())
                .userId(auditLog.getUserId())
                .username(auditLog.getUsername())
                .type(auditLog.getOperationType())
                .detail(auditLog.getOperationDetail())
                .extraInfo(auditLog.getExtraInfo())
                .time(auditLog.getOperationTime())
                .result(auditLog.getOperationResult())
                .ip(auditLog.getIp())
                .ipRegion(auditLog.getIpRegion())
                .osType(auditLog.getOsType())
                .browserType(auditLog.getBrowserType())
                .deviceType(auditLog.getDeviceType())
                .requestId(auditLog.getRequestId())
                .build())
                .toList();
        pageData.setList(records);
        return pageData;
    }

    /**
     * 获取系统操作日志
     *
     * @param page
     *            页数
     * @param size
     *            条数
     * @param keyword
     *            用户ID / 用户名 / IP 检索关键字
     * @param type
     *            操作类型
     * @param resourceId
     *            资源ID
     * @param start
     *            开始时间
     * @param end
     *            结束时间
     * @return 系统操作日志
     */
    @Override
    public PageData<AuditLogResponseDto> getSysOperationLogs(int page, int size, String keyword, Integer type,
            String resourceId, LocalDateTime start, LocalDateTime end) {
        // 1. 数据库操作
        Page<AuditLog> pageRequest = new Page<>(page, size);
        auditLogRepository.searchSysOperationLog(pageRequest, keyword, type, resourceId, start, end);

        // 2. 属性设置
        PageData<AuditLogResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var records = CommonUtil.stream(pageRequest.getRecords()).map(auditLog -> AuditLogResponseDto.builder()
                .id(auditLog.getAuditId())
                .userId(auditLog.getUserId())
                .username(auditLog.getUsername())
                .type(auditLog.getOperationType())
                .resourceId(auditLog.getResourceId())
                .detail(auditLog.getOperationDetail())
                .extraInfo(auditLog.getExtraInfo())
                .time(auditLog.getOperationTime())
                .result(auditLog.getOperationResult())
                .ip(auditLog.getIp())
                .ipRegion(auditLog.getIpRegion())
                .osType(auditLog.getOsType())
                .browserType(auditLog.getBrowserType())
                .deviceType(auditLog.getDeviceType())
                .requestId(auditLog.getRequestId())
                .build())
                .toList();
        pageData.setList(records);
        return pageData;
    }
}
