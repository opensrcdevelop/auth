package cn.opensrcdevelop.common.exception;

import jakarta.validation.ConstraintViolation;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Set;

@RequiredArgsConstructor
@Getter
public class ValidationException extends RuntimeException {

    private final Set<ConstraintViolation<Object>> constraintViolations;
}
