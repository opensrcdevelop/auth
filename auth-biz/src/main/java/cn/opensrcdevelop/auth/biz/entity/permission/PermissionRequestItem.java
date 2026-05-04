package cn.opensrcdevelop.auth.biz.entity.permission;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_permission_request_item")
@EntityName("权限申请明细")
public class PermissionRequestItem extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 512879806882171328L;

    @TableId(type = IdType.INPUT)
    @PropertyName("明细ID")
    private String itemId;

    @PropertyName("申请ID")
    private String requestId;

    @PropertyName("权限ID")
    private String permissionId;

    @PropertyName("审批状态")
    private String status;

    @PropertyName("拒绝理由")
    private String rejectReason;

    @PropertyName("审批人ID")
    private String approverId;

    @PropertyName("审批时间")
    private LocalDateTime approveTime;

    /** 审批人用户名 */
    @TableField(exist = false)
    private String approverUsername;

    @TableField(exist = false)
    private Permission permission;
}
