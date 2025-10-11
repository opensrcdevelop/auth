package cn.opensrcdevelop.auth.audit.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("t_audit_log")
public class AuditLog {

    @TableId(type = IdType.INPUT)
    private String auditId;

    /** 请求ID */
    private String requestId;

    /** 用户ID */
    private String userId;

    /** 审计类型 */
    private Integer auditType;

    /** 操作类型 */
    private Integer operationType;

    /** 资源ID */
    private String resourceId;

    /** 操作结果 */
    private Boolean operationResult;

    /** 操作时间 */
    private LocalDateTime operationTime;

    /** 操作详情 */
    private String operationDetail;

    /** IP */
    private String ip;

    /** IP地区  */
    private String ipRegion;

    /** 设备类型 */
    private String deviceType;

    /** 操作系统类型 */
    private String osType;

    /** 浏览器类型 */
    private String browserType;

    /** 额外信息 */
    private String extraInfo;

    @TableField(exist = false)
    private String username;
}
