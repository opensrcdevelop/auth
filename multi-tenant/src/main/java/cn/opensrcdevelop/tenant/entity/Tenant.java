package cn.opensrcdevelop.tenant.entity;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.FieldStrategy;
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
@TableName("t_tenant")
@EntityName("租户")
public class Tenant extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 6458896905734013348L;

    @TableId(type = IdType.INPUT)
    @PropertyName("租户ID")
    private String tenantId;

    @PropertyName("租户标识")
    private String tenantCode;

    @PropertyName("租户名称")
    private String tenantName;

    @PropertyName("描述")
    private String description;

    @PropertyName("是否启用")
    private Boolean enabled;

    @PropertyName("生效时间")
    @TableField(updateStrategy = FieldStrategy.ALWAYS)
    private LocalDateTime effectiveTime;

    @PropertyName("失效时间")
    @TableField(updateStrategy = FieldStrategy.ALWAYS)
    private LocalDateTime expirationTime;
}
