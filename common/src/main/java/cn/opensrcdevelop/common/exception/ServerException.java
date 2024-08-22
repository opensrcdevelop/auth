package cn.opensrcdevelop.common.exception;

/**
 * 服务异常
 */
public class ServerException extends RuntimeException {

    public ServerException(String msg, Throwable ex) {
        super(msg, ex);
    }

    public ServerException(Throwable ex) {
        super(ex);
    }
}
