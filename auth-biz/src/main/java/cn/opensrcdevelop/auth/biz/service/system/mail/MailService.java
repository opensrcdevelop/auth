package cn.opensrcdevelop.auth.biz.service.system.mail;

import cn.opensrcdevelop.auth.biz.entity.system.password.PasswordPolicy;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import java.time.LocalDateTime;

public interface MailService {

    void sendMailCode(String to);

    void sendCreateUserNotice(String to, String username, String password);

    void sendResetPwdNotice(String to, String username, String password);

    void sendBindEmailCode(String to);

    void sendRemindUpdatePwd(User user, PasswordPolicy passwordPolicy, LocalDateTime expireTime,
            LocalDateTime executeTime);
}
