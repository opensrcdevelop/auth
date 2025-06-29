package cn.opensrcdevelop.auth.biz.entity.role;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroup;
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
 * 角色实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_role")
@EntityName("角色")
public class Role extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 2220067046710884826L;

    /** 角色ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("角色ID")
    private String roleId;

    /** 角色名称 */
    @PropertyName("角色名称")
    private String roleName;

    /** 角色标识 */
    @PropertyName("角色标识")
    private String roleCode;

    /** 描述 */
    @PropertyName("描述")
    private String description;

    /** 用户集合 */
    @TableField(exist = false)
    private List<User> users;

    /** 用户组集合 */
    @TableField(exist = false)
    private List<UserGroup> userGroups;
}
