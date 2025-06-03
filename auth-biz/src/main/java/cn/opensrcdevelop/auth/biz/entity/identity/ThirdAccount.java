package cn.opensrcdevelop.auth.biz.entity.identity;

import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

/**
 * 第三方账号实体
 *
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("t_third_account")
public class ThirdAccount extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -495384309561604976L;

    /** 用户 ID */
    private String userId;

    /** 身份源注册 ID */
    private String registrationId;

    /** 第三方账号唯一标识 */
    private String uniqueId;

    /** 第三方账号信息 */
    private String details;

    @TableField(exist = false)
    private User user;
}
