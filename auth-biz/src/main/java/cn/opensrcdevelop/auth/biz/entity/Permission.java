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
 * 权限实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_permission")
public class Permission extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 681251509735491818L;

    /** 权限ID */
    @TableId(type = IdType.INPUT)
    private String permissionId;

    /** 权限名 */
    private String permissionName;

    /** 权限码 */
    private String permissionCode;

    /** 描述 */
    private String description;

    /** 资源ID */
    private String resourceId;

    /** 资源 */
    @TableField(exist = false)
    private Resource resource;
}
