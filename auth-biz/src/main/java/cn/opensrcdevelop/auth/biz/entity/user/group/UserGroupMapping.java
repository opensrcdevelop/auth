package cn.opensrcdevelop.auth.biz.entity.user.group;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

/**
 * 户组映射实体
 */
@Data
@TableName("t_user_group_mapping")
public class UserGroupMapping {

    /** 用户ID */
    private String userId;

    /** 用户组ID */
    private String userGroupId;
}
