package cn.opensrcdevelop.auth.client.exception;

public class AuthClientException extends RuntimeException {

    public AuthClientException(Throwable e, String msg) {
        super(msg, e);
    }

    public AuthClientException(String msg) {
        super(msg);
    }
}
