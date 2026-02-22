package cn.opensrcdevelop.common.aop;

import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.exception.ValidationException;
import cn.opensrcdevelop.common.response.CodeEnum;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.response.ValidationErrorResponse;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.MessageUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.text.MessageFormat;
import java.util.Collection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.http.HttpStatus;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.HandlerMethodValidationException;
import org.springframework.web.servlet.resource.NoResourceFoundException;

@RestControllerAdvice
@RequiredArgsConstructor
@Slf4j
public class RestExceptionHandler {

    private static final String BIZ_LOG_MSG = "MsgCode: {0}, Msg: {1}";
    private final MessageUtil messageUtil;

    /**
     * 参数校验错误
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    @ResponseStatus(HttpStatus.UNPROCESSABLE_ENTITY)
    public R<ValidationErrorResponse> exception(MethodArgumentNotValidException e) {
        log.debug(e.getMessage(), e);
        ValidationErrorResponse response = new ValidationErrorResponse();
        BindingResult bindingResult = e.getBindingResult();
        response.setErrors(bindingResult.getFieldErrors().stream().map(o -> {
            var error = new ValidationErrorResponse.ValidationError();
            error.setField(o.getField());
            error.setErrorMsg(o.getDefaultMessage());

            return error;
        }).toList());
        return R.optFailWithData(CodeEnum.RCD20001, response);
    }

    /**
     * 参数校验错误
     */
    @ExceptionHandler(HandlerMethodValidationException.class)
    @ResponseStatus(HttpStatus.UNPROCESSABLE_ENTITY)
    public R<ValidationErrorResponse> exception(HandlerMethodValidationException e) {
        log.debug(e.getMessage(), e);
        ValidationErrorResponse response = new ValidationErrorResponse();
        response.setErrors(e.getParameterValidationResults().stream().map(o -> {
            String paramName = o.getMethodParameter().getParameterName();
            return o.getResolvableErrors().stream().map(x -> {
                var error = new ValidationErrorResponse.ValidationError();
                if (x instanceof FieldError fieldError) {
                    error.setField(fieldError.getField());
                } else {
                    error.setField(paramName);
                }
                error.setErrorMsg(x.getDefaultMessage());

                return error;
            }).toList();
        }).flatMap(Collection::stream).toList());
        return R.optFailWithData(CodeEnum.RCD20001, response);
    }

    /**
     * 参数校验错误
     */
    @ExceptionHandler(ValidationException.class)
    @ResponseStatus(HttpStatus.UNPROCESSABLE_ENTITY)
    public R<ValidationErrorResponse> exception(ValidationException e) {
        ValidationErrorResponse response = new ValidationErrorResponse();
        response.setErrors(CommonUtil.stream(e.getConstraintViolations()).map(c -> {
            var error = new ValidationErrorResponse.ValidationError();
            error.setField(c.getPropertyPath().toString());
            error.setErrorMsg(c.getMessage());

            return error;
        }).toList());
        return R.optFailWithData(CodeEnum.RCD20001, response);
    }

    /**
     * 业务异常
     */
    @ExceptionHandler(BizException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public R<Object> exception(BizException e) {
        String msgCode = e.getMsgCode();
        log.info(MessageFormat.format(BIZ_LOG_MSG, msgCode, messageUtil.getMsg(msgCode, e.getParams())), e);
        return R.optFail(msgCode, e.getParams());
    }

    /**
     * 404
     */
    @ExceptionHandler({NoResourceFoundException.class, HttpRequestMethodNotSupportedException.class})
    @ResponseStatus(HttpStatus.NOT_FOUND)
    public R<Object> exception404(Exception e, HttpServletRequest request) {
        log.debug(e.getMessage(), e);
        return R.optFail(CodeEnum.RCD40004, request.getRequestURI());
    }

    /**
     * 键重复
     */
    @ExceptionHandler(DuplicateKeyException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public R<Object> exception(DuplicateKeyException e) {
        log.debug(e.getMessage(), e);
        return R.optFail(CodeEnum.RCD40005);
    }

    /**
     * 未授权
     */
    @ExceptionHandler(AccessDeniedException.class)
    @ResponseStatus(HttpStatus.FORBIDDEN)
    public R<Object> exception(AccessDeniedException e) {
        log.debug(e.getMessage(), e);
        return R.optFail(CodeEnum.RCD40003);
    }

    /**
     * OAuth2 未认证
     */
    @ExceptionHandler(OAuth2AuthenticationException.class)
    @ResponseStatus(HttpStatus.UNAUTHORIZED)
    public R<Object> exception(HttpServletRequest request, OAuth2AuthenticationException e) {
        log.debug(e.getMessage(), e);
        var session = request.getSession();
        if (session != null) {
            session.invalidate();
        }
        return R.optFail(CodeEnum.RCD40001);
    }

    /**
     * JSON 反序列异常
     */
    @ExceptionHandler(HttpMessageNotReadableException.class)
    @ResponseStatus(HttpStatus.UNPROCESSABLE_ENTITY)
    public R<Object> exception(HttpMessageNotReadableException e) {
        log.debug(e.getMessage(), e);
        return R.optFail(CodeEnum.RCD20001);
    }

    /**
     * 系统内部错误
     */
    @ExceptionHandler({Exception.class, ServerException.class})
    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    public R<Object> exception(Exception e, HttpServletResponse response) {
        // 如果 response 已经提交或 getOutputStream 已被调用，直接返回 null
        if (response.isCommitted()) {
            log.error("Response already committed, skipping error response. Original error: {}", e.getMessage(), e);
            return null;
        }
        log.error(e.getMessage(), e);
        return R.internalFail();
    }
}
