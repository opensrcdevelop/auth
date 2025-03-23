package cn.opensrcdevelop.auth.biz.service;

public interface MailService {

    void sendMailCode(String to);

    void sendCreateUserNotice(String to, String username, String password);

    void sendResetPwdNotice(String to, String username, String password);

    void sendBindEmailCode(String to);
}
