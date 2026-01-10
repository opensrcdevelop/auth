package cn.opensrcdevelop.auth.biz.entity.resource;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.auth.biz.entity.permission.Permission;
import cn.opensrcdevelop.auth.biz.entity.resource.group.ResourceGroup;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import java.util.List;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 资源实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_resource")
@EntityName("资源")
public class Resource extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 6431435600870053441L;

    /** 资源ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("资源ID")
    private String resourceId;

    /** 资源名称 */
    @PropertyName("资源名称")
    private String resourceName;

    /** 资源标识 */
    @PropertyName("资源标识")
    private String resourceCode;

    /** 描述 */
    @PropertyName("描述")
    private String description;

    /** API URL 标识 */
    @PropertyName("API URL 标识")
    private String apiIdentifier;

    /** 资源组ID */
    @PropertyName("资源组ID")
    private String resourceGroupId;

    /** 权限集合 */
    @TableField(exist = false)
    private List<Permission> permissions;

    /** 资源组 */
    @TableField(exist = false)
    private ResourceGroup resourceGroup;
}
