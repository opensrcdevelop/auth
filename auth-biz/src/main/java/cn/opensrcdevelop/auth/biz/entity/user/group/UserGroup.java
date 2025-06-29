package cn.opensrcdevelop.auth.biz.entity.user.group;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.auth.biz.entity.user.User;
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
 * 用户组实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_user_group")
@EntityName("用户组")
public class UserGroup extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -6540706315180961918L;

    /** 用户组ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("用户组ID")
    private String userGroupId;

    /** 用户组名 */
    @PropertyName("用户组名称")
    private String userGroupName;

    /** 用户组码 */
    @PropertyName("用户组标识")
    private String userGroupCode;

    /** 描述 */
    @PropertyName("描述")
    private String description;

    /** 用户集合 */
    @TableField(exist = false)
    private List<User> users;
}
