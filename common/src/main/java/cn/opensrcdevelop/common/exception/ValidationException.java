package cn.opensrcdevelop.common.exception;

import jakarta.validation.ConstraintViolation;
import java.util.Set;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public class ValidationException extends RuntimeException {

    private final Set<ConstraintViolation<Object>> constraintViolations;
}
