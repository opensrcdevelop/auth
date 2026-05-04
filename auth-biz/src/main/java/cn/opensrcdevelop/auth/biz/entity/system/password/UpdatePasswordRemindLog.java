package cn.opensrcdevelop.auth.biz.entity.system.password;

import cn.opensrcdevelop.auth.biz.entity.user.User;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import lombok.Data;

/**
 * 密码到期修改提醒日志
 */
@TableName("t_update_password_remind_log")
@Data
public class UpdatePasswordRemindLog implements Serializable {

    @Serial
    private static final long serialVersionUID = -7430027239181604992L;

    /** 主键 ID */
    @TableId(type = IdType.INPUT)
    private String remindLogId;

    /** 用户ID */
    private String userId;

    /** 密码策略ID */
    private String policyId;

    /** 提醒方式 */
    private String remindMethod;

    /** 提醒时间 */
    private LocalDateTime remindTime;

    /** 是否成功 */
    private boolean success;

    @TableField(exist = false)
    private User user;

    @TableField(exist = false)
    private PasswordPolicy passwordPolicy;
}
