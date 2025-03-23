package cn.opensrcdevelop.auth.biz.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 授权记录实体
 */
@Data
@TableName("t_authorize")
public class AuthorizeRecord implements Serializable {

    @Serial
    private static final long serialVersionUID = 3705715660300297013L;

    /** 授权ID */
    @TableId(type = IdType.INPUT)
    private String authorizeId;

    /** 用户ID */
    private String userId;

    /** 角色ID */
    private String roleId;

    /** 用户组ID */
    private String userGroupId;

    /** 权限ID */
    private String permissionId;

    /** 优先级 */
    private Integer priority;

    /** 授权时间 */
    private LocalDateTime authorizeTime;

    /** 用户 */
    @TableField(exist = false)
    private User user;

    /** 角色 */
    @TableField(exist = false)
    private Role role;

    /** 用户组 */
    @TableField(exist = false)
    private UserGroup userGroup;

    /** 权限 */
    @TableField(exist = false)
    private Permission permission;

    /** 权限表达式 */
    @TableField(exist = false)
    private List<PermissionExp> permissionExps;
}
