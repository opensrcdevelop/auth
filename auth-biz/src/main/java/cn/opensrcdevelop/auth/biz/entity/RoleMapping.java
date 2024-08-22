package cn.opensrcdevelop.auth.biz.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

/**
 * 角色映射实体
 */
@Data
@TableName("t_role_mapping")
public class RoleMapping {

    /** 角色ID */
    private String roleId;

    /** 用户ID */
    private String userId;

    /** 用户组ID */
    private String userGroupId;

    @TableField(exist = false)
    private Role role;

    @TableField(exist = false)
    private User user;

    @TableField(exist = false)
    private UserGroup userGroup;
}
