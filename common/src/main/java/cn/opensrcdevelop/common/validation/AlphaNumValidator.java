package cn.opensrcdevelop.common.validation;

import cn.opensrcdevelop.common.validation.constraints.AlphaNum;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

import java.util.regex.Pattern;

public class AlphaNumValidator implements ConstraintValidator<AlphaNum, CharSequence> {

    private static final String PATTERN_PREFIX = "^[A-Za-z0-9";
    private static final String PATTERN_SUFFIX = "]+$";
    private boolean allowUnderline;
    private boolean allowHyphen;

    @Override
    public void initialize(AlphaNum constraintAnnotation) {
        allowUnderline = constraintAnnotation.allowUnderline();
        allowHyphen = constraintAnnotation.allowHyphen();
    }

    @Override
    public boolean isValid(CharSequence value, ConstraintValidatorContext context) {
        if (StringUtils.isNotEmpty(value)) {
            StringBuilder pattern = new StringBuilder(PATTERN_PREFIX);
            if (allowHyphen) {
                pattern.append("-");
            }

            if (allowUnderline) {
                pattern.append("_");
            }
            pattern.append(PATTERN_SUFFIX);

            return Pattern.compile(pattern.toString()).matcher(value).matches();
        }
        return true;
    }
}
