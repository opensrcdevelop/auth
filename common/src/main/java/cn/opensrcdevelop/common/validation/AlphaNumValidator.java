package cn.opensrcdevelop.common.validation;

import cn.opensrcdevelop.common.validation.constraints.AlphaNum;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

import java.util.regex.Pattern;

public class AlphaNumValidator implements ConstraintValidator<AlphaNum, CharSequence> {

    private static final Pattern PATTERN = Pattern.compile("^[A-Za-z0-9-_]+$");

    @Override
    public boolean isValid(CharSequence value, ConstraintValidatorContext context) {
        if (StringUtils.isNotEmpty(value)) {
            return PATTERN.matcher(value).matches();
        }
        return true;
    }
}
