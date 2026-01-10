package cn.opensrcdevelop.common.validation.constraints;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import cn.opensrcdevelop.common.validation.AlphaNumValidator;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Constraint(validatedBy = AlphaNumValidator.class)
@Target({METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE})
@Retention(RUNTIME)
@Documented
public @interface AlphaNum {

    // 允许出现下划线 _
    boolean allowUnderline() default true;
    // 允许出现短横线 -
    boolean allowHyphen() default true;
    // 仅允许小写字母
    boolean onlyLowerCaseLetter() default false;

    String message() default "{cn.opensrcdevelop.common.validation.constraints.AlphaNum.message}";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
