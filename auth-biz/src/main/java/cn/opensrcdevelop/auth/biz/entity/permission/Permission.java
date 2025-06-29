package cn.opensrcdevelop.auth.biz.entity.permission;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.auth.biz.entity.resource.Resource;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

/**
 * 权限实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_permission")
@EntityName("权限")
public class Permission extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 681251509735491818L;

    /** 权限ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("权限ID")
    private String permissionId;

    /** 权限名称 */
    @PropertyName("权限名称")
    private String permissionName;

    /** 权限标识 */
    @PropertyName("权限标识")
    private String permissionCode;

    /** 描述 */
    @PropertyName("描述")
    private String description;

    /** 资源ID */
    @PropertyName("资源ID")
    private String resourceId;

    /** 资源 */
    @TableField(exist = false)
    private Resource resource;
}
