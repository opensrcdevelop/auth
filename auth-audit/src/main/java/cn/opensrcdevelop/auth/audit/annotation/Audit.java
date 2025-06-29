package cn.opensrcdevelop.auth.audit.annotation;

import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import org.apache.commons.lang3.StringUtils;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD })
public @interface Audit {

    /**
     * 审计类型
     */
    AuditType type();

    /**
     * 资源类型
     */
    ResourceType resource();

    /**
     * 系统操作类型
     */
    SysOperationType sysOperation() default SysOperationType.UNKNOWN;

    /**
     * 用户操作类型
     */
    UserOperationType userOperation() default UserOperationType.UNKNOWN;

    /**
     * 成功消息
     */
    String success() default StringUtils.EMPTY;

    /**
     * 错误信息
     */
    String error() default StringUtils.EMPTY;

    /**
     *  额外信息
     */
    String extra() default StringUtils.EMPTY;
}
