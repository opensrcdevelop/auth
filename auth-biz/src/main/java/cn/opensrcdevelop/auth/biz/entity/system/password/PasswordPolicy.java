package cn.opensrcdevelop.auth.biz.entity.system.password;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_password_policy")
public class PasswordPolicy extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -5926379639240532329L;

    /** 策略 ID */
    @TableId(type = IdType.INPUT)
    private String policyId;

    /** 策略名称 */
    private String policyName;

    /** 策略描述 */
    private String description;

    /** 密码强度 */
    private Integer passwordStrength;

    /** 自定义密码强度配置 */
    private String customStrengthConfig;

    /** 是否启用 */
    private Boolean enabled;

    /** 优先级 */
    private Integer priority;

    /** 是否开启用户登录密码强度检查 */
    private Boolean enablePasswordDetection;

    /** 是否开启强制修改密码 */
    private Boolean enableForceChangePassword;

    /** 强制修改密码周期 */
    private Integer forcedCycle;

    /** 强制修改密码周期单位 */
    private String forcedCycleUnit;

    /** 密码到期提醒周期 */
    private Integer remindCycle;

    /** 密码到期提醒周期单位 */
    private String remindCycleUnit;
}
