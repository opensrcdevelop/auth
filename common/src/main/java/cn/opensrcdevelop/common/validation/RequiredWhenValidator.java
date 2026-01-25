package cn.opensrcdevelop.common.validation;

import cn.opensrcdevelop.common.validation.constraints.RequiredWhen;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Objects;

/**
 * 条件校验Validator 当依赖字段的值为指定值之一时，当前字段必须填写
 */
public class RequiredWhenValidator implements ConstraintValidator<RequiredWhen, Object> {

    private String dependentField;

    private String[] whenValues;

    @Override
    public void initialize(RequiredWhen constraintAnnotation) {
        this.dependentField = constraintAnnotation.dependentField();
        this.whenValues = constraintAnnotation.whenValues();
    }

    @Override
    public boolean isValid(Object value, ConstraintValidatorContext context) {
        if (value == null) {
            return true;
        }

        // 获取依赖字段的值
        try {
            Field dependentFieldObj = value.getClass().getDeclaredField(dependentField);
            dependentFieldObj.setAccessible(true);
            Object dependentValue = dependentFieldObj.get(value);

            // 检查依赖字段的值是否在指定值列表中
            boolean shouldBeRequired = Arrays.stream(whenValues)
                    .anyMatch(v -> Objects.equals(v, dependentValue));

            if (shouldBeRequired) {
                // 获取当前字段的值（通过反射获取对象中的值字段）
                Field valueField = value.getClass().getDeclaredField("value");
                valueField.setAccessible(true);
                Object actualValue = valueField.get(value);

                if (actualValue == null) {
                    // 构建动态消息
                    String messageTemplate = "当筛选条件为" + Arrays.toString(whenValues) + "时，值不能为空";
                    context.disableDefaultConstraintViolation();
                    context.buildConstraintViolationWithTemplate(messageTemplate)
                            .addPropertyNode("value")
                            .addConstraintViolation();
                    return false;
                }
            }
        } catch (NoSuchFieldException | IllegalAccessException e) {
            // 字段不存在或无法访问，跳过校验
            return true;
        }

        return true;
    }
}
