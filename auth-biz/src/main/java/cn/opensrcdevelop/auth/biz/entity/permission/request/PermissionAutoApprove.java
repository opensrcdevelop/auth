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
@TableName("t_permission_auto_approve")
@EntityName("自动批准配置")
public class PermissionAutoApprove extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.INPUT)
    @PropertyName("配置ID")
    private String id;

    @PropertyName("租户ID")
    private String tenantId;

    @PropertyName("权限ID")
    private String permissionId;

    @PropertyName("是否启用")
    private Boolean enabled;
}
