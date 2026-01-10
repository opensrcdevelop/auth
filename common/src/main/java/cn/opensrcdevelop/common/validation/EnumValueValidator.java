package cn.opensrcdevelop.common.validation;

import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 枚举校验
 */
public class EnumValueValidator implements ConstraintValidator<EnumValue, Object> {

    private Set<String> stringValues;

    @Override
    public void initialize(EnumValue constraintAnnotation) {
        stringValues = Arrays.stream(constraintAnnotation.value()).distinct().collect(Collectors.toSet());
    }

    @Override
    public boolean isValid(Object value, ConstraintValidatorContext context) {
        // 集合类型
        if (value instanceof Collection<?> collectionValue) {
            return collectionValue.stream().noneMatch(x -> {
                if (x instanceof String) {
                    return !stringValues.contains(x);
                }
                return false;
            });
        }

        // 字符串类型
        if (value instanceof String strValue) {
            return stringValues.contains(strValue);
        }

        // 数值类型
        if (value instanceof Number numberValue) {
            return stringValues.contains(numberValue.toString());
        }

        return true;
    }
}
