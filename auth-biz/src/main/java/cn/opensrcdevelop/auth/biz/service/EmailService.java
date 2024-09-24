package cn.opensrcdevelop.auth.biz.service;

public interface EmailService {

    void sendEmailCode(String to);

    void sendCreateUserNotice(String to, String username, String password);

    void sendResetPwdNotice(String to, String username, String password);

    void sendBindEmailCode(String to);
}
