package cn.opensrcdevelop.common.exception;

import lombok.Getter;

/**
 * 业务异常
 */
@Getter
public class BizException extends RuntimeException {

    /** 业务消息码 */
    private final String msgCode;
    /** 消息参数 */
    private final transient Object[] params;

    public BizException(Throwable e, String msgCode, Object... params) {
        super(e.getMessage(), e);
        this.msgCode = msgCode;
        this.params = params;
    }

    public BizException(Throwable e, String msgCode) {
        super(e.getMessage(), e);
        this.msgCode = msgCode;
        this.params = new Object[]{};
    }

    public BizException(String msgCode, Object... params) {
        this.msgCode = msgCode;
        this.params = params;
    }

    public BizException(String msgCode) {
        this.msgCode = msgCode;
        this.params = new Object[]{};
    }
}
