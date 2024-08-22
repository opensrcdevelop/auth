package cn.opensrcdevelop.common.response;

import lombok.Data;

import java.util.List;

/**
 * 参数检验错误响应
 */
@Data
public class ValidationErrorResponse {

    /** 参数校验错误响应 */
    private List<ValidationError> errors;

    @Data
    public static class ValidationError {

        /** 字段 */
        private String field;

        /** 错误消息 */
        private String errorMsg;
    }
}
