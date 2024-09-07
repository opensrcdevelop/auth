package cn.opensrcdevelop.tenant.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_tenant")
public class Tenant extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 6458896905734013348L;

    @TableId(type = IdType.INPUT)
    private String tenantId;

    private String tenantCode;

    private String tenantName;

    private String description;

    private Boolean enabled;
}
