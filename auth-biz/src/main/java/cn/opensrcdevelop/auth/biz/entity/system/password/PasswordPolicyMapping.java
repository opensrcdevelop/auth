package cn.opensrcdevelop.auth.biz.entity.system.password;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Data
@TableName("t_password_policy_mapping")
public class PasswordPolicyMapping implements Serializable {

    @Serial
    private static final long serialVersionUID = 3517136991901249137L;

    /** 策略 ID */
    private String policyId;

    /** 用户 ID */
    private String userId;

    /** 用户组 ID */
    private String userGroupId;
}
