package cn.opensrcdevelop.auth.audit.util;

import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.auth.audit.event.AuditEvent;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;

import java.time.LocalDateTime;

public class AuditUtil {

    private AuditUtil() {}

    /**
     * 发布用户操作审计事件（成功）
     *
     * @param userId 用户ID
     * @param resourceType 资源类型
     * @param userOperationType 用户操作类型
     * @param success 成功消息
     */
    public static void publishSuccessUserOperationAuditEvent(String userId, ResourceType resourceType, UserOperationType userOperationType, String success) {
        AuditLog auditLog = new AuditLog();
        auditLog.setAuditId(CommonUtil.getUUIDV7String());
        auditLog.setUserId(userId);
        auditLog.setAuditType(AuditType.USER_OPERATION.ordinal());
        auditLog.setResourceId(resourceType.getId());
        auditLog.setOperationType(userOperationType.ordinal());
        auditLog.setOperationResult(true);
        auditLog.setOperationDetail(success);

        String ip = WebUtil.getRemoteIP();
        auditLog.setIp(ip);
        auditLog.setIpRegion(WebUtil.getIpRegion(ip));
        auditLog.setOsType(WebUtil.getDeviceOs());
        auditLog.setDeviceType(WebUtil.getDeviceType());
        auditLog.setBrowserType(WebUtil.getBrowserType());

        auditLog.setRequestId(WebUtil.getRequestId());
        auditLog.setOperationTime(LocalDateTime.now());

        SpringContextUtil.publishEvent(new AuditEvent(auditLog, null));
    }
}
