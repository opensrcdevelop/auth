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
@TableName("t_permission_request_cond")
@EntityName("权限审批限制条件")
public class PermissionRequestCond extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.INPUT)
    @PropertyName("条件ID")
    private String condId;

    @PropertyName("申请ID")
    private String requestId;

    @PropertyName("明细ID")
    private String itemId;

    @PropertyName("表达式ID")
    private String expId;
}
