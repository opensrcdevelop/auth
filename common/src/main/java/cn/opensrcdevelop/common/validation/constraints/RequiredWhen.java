package cn.opensrcdevelop.common.validation.constraints;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import cn.opensrcdevelop.common.validation.RequiredWhenValidator;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * 条件校验注解 当依赖字段的值在指定值列表中时，当前字段必须填写
 */
@Constraint(validatedBy = RequiredWhenValidator.class)
@Target({METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE})
@Retention(RUNTIME)
@Documented
public @interface RequiredWhen {

    String message() default "{cn.opensrcdevelop.common.validation.constraints.RequiredWhen.message}";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    /**
     * 依赖的字段名
     */
    String dependentField();

    /**
     * 当依赖字段值为以下值时，当前字段必须填写
     */
    String[] whenValues();
}
