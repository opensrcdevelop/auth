package cn.opensrcdevelop.common.response;

import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.MessageUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.time.LocalDateTime;
import lombok.Data;

@Data
public class R<T> {

    // 调用结果状态
    private Boolean success;

    // 响应码
    private Integer code;

    // 详细信息
    private String message;

    // 时间
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS)
    private LocalDateTime time = LocalDateTime.now();

    // 响应数据
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private T data;

    public static <T> R<T> ok() {
        R<T> r = new R<>();
        r.setCode(CodeEnum.RCD0.getCode());
        r.setMessage(CodeEnum.RCD0.getMessage());
        r.setSuccess(true);
        return r;
    }

    public static <T> R<T> ok(T data) {
        R<T> r = new R<>();
        r.setCode(CodeEnum.RCD0.getCode());
        r.setMessage(CodeEnum.RCD0.getMessage());
        r.setSuccess(true);
        r.setData(data);
        return r;
    }

    public static <T> R<T> optFail() {
        R<T> r = new R<>();
        r.setCode(CodeEnum.RCD20000.getCode());
        r.setMessage(CodeEnum.RCD20000.getMessage());
        r.setSuccess(false);
        return r;
    }

    public static <T> R<T> optFail(CodeEnum codeEnum, Object... params) {
        R<T> r = new R<>();
        r.setCode(codeEnum.getCode());
        r.setMessage(getMsg(String.valueOf(codeEnum.getCode()), params));
        r.setSuccess(false);
        return r;
    }

    public static <T> R<T> optFail(String msgCode, Object... params) {
        R<T> r = new R<>();
        r.setCode(CodeEnum.RCD20000.getCode());
        r.setMessage(getMsg(msgCode, params));
        r.setSuccess(false);
        return r;
    }

    public static <T> R<T> optFailWithData(T data) {
        R<T> r = new R<>();
        r.setCode(CodeEnum.RCD20000.getCode());
        r.setMessage(CodeEnum.RCD20000.getMessage());
        r.setSuccess(false);
        r.setData(data);
        return r;
    }

    public static <T> R<T> optFail(CodeEnum codeEnum) {
        R<T> r = new R<>();
        r.setCode(codeEnum.getCode());
        r.setMessage(codeEnum.getMessage());
        r.setSuccess(false);
        return r;
    }

    public static <T> R<T> optFailWithData(CodeEnum codeEnum, T data) {
        R<T> r = new R<>();
        r.setCode(codeEnum.getCode());
        r.setMessage(codeEnum.getMessage());
        r.setData(data);
        r.setSuccess(false);
        return r;
    }

    public static <T> R<T> optFailWithData(T data, String msgCode, Object... params) {
        R<T> r = new R<>();
        r.setCode(CodeEnum.RCD20000.getCode());
        r.setMessage(getMsg(msgCode, params));
        r.setSuccess(false);
        r.setData(data);
        return r;
    }

    public static <T> R<T> internalFail() {
        R<T> r = new R<>();
        r.setCode(CodeEnum.RCD50000.getCode());
        r.setMessage(CodeEnum.RCD50000.getMessage());
        r.setSuccess(false);
        return r;
    }

    private static String getMsg(String msgCode, Object... args) {
        MessageUtil messageUtil = SpringContextUtil.getBean(MessageUtil.class);
        return messageUtil.getMsg(msgCode, args);
    }
}
