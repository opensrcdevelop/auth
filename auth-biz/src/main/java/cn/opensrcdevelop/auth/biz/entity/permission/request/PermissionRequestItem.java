package cn.opensrcdevelop.auth.biz.entity.permission.request;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_permission_request_item")
@EntityName("权限申请明细")
public class PermissionRequestItem extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.INPUT)
    @PropertyName("明细ID")
    private String itemId;

    @PropertyName("申请ID")
    private String requestId;

    @PropertyName("权限ID")
    private String permissionId;

    @PropertyName("是否自动批准")
    private Boolean autoApproved;

    @PropertyName("限制条件ID列表")
    private String restrictionIds;
}