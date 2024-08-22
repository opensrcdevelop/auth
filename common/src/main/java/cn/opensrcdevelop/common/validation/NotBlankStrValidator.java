package cn.opensrcdevelop.common.validation;

import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

/**
 * 非空白字符串校验
 */
public class NotBlankStrValidator implements ConstraintValidator<NotBlankStr, CharSequence> {

    @Override
    public boolean isValid(CharSequence value, ConstraintValidatorContext context) {
        if (value != null) {
            return !value.toString().trim().isEmpty();
        }
        return true;
    }
}
