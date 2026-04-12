package cn.opensrcdevelop.auth.biz.entity.permission.request;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_permission_request")
@EntityName("权限申请")
public class PermissionRequest extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.INPUT)
    @PropertyName("申请ID")
    private String requestId;

    @PropertyName("申请人ID")
    private String userId;

    @PropertyName("申请理由")
    private String reason;

    @PropertyName("申请状态")
    private String status;

    @PropertyName("申请时间")
    private LocalDateTime requestTime;

    @PropertyName("审批人ID")
    private String approverId;

    @PropertyName("审批时间")
    private LocalDateTime approveTime;

    @PropertyName("拒绝理由")
    private String rejectReason;
}
