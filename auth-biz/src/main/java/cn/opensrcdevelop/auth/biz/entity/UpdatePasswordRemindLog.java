package cn.opensrcdevelop.auth.biz.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * 密码到期修改提醒日志
 */
@TableName("t_update_password_remind_log")
@Data
public class UpdatePasswordRemindLog implements Serializable {

    @Serial
    private static final long serialVersionUID = -7430027239181604992L;

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
