package cn.opensrcdevelop.common.validation.constraints;

import cn.opensrcdevelop.common.validation.AlphaNumValidator;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.ElementType.TYPE_USE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Constraint(validatedBy = AlphaNumValidator.class)
@Target({ METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE })
@Retention(RUNTIME)
@Documented
public @interface AlphaNum {

    // 允许出现下划线 _
    boolean allowUnderline() default true;
    // 允许出现短横线 -
    boolean allowHyphen() default true;

    String message() default "{cn.opensrcdevelop.common.validation.constraints.AlphaNum.message}";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
