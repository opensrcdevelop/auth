package cn.opensrcdevelop.auth.biz.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

/**
 * 资源实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_resource")
public class Resource extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 6431435600870053441L;

    /** 资源ID */
    @TableId(type = IdType.INPUT)
    private String resourceId;

    /** 资源名称 */
    private String resourceName;

    /** 资源码 */
    private String resourceCode;

    /** 描述 */
    private String description;

    /** API URL 标识 */
    private String apiIdentifier;

    /** 资源组ID */
    private String resourceGroupId;

    /** 权限集合 */
    @TableField(exist = false)
    private List<Permission> permissions;

    /** 资源组 */
    @TableField(exist = false)
    private ResourceGroup resourceGroup;
}
