package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.service.EmailService;
import cn.opensrcdevelop.auth.biz.service.UserService;
import cn.opensrcdevelop.auth.biz.service.VerificationCodeService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.entity.MailInfo;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.MailUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.stereotype.Service;

import java.text.MessageFormat;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Service
public class EmailServiceImpl implements EmailService {

    private static final String DEFAULT_EMAIL_CODE_SUBJECT = "请查收您的验证码";
    private static final String DEFAULT_CREATE_USER_NOTICE_SUBJECT = "账号创建通知";
    private static final String DEFAULT_RESET_PWD_NOTICE_SUBJECT = "密码重置通知";
    private static final String DEFAULT_FROM_FORMAT = "{0}<{1}>";
    private static final String EMAIL_CODE_TEMPLATE_PATH = "templates/emilaCode.html.ftl";
    private static final String CREATE_USER_NOTICE_PATH = "templates/createUserNotice.html.ftl";
    private static final String RESET_PWD_NOTICE_PATH = "templates/resetPwdNotice.html.ftl";
    private static final String BIND_EMAIL_TEMPLATE_PATH = "templates/bindEmail.html.ftl";

    private final JavaMailSender mailSender;
    private final UserService userService;
    private final VerificationCodeService verificationCodeService;

    @SuppressWarnings("all")
    public EmailServiceImpl(JavaMailSender mailSender, @Lazy UserService userService, VerificationCodeService verificationCodeService) {
        this.mailSender = mailSender;
        this.userService = userService;
        this.verificationCodeService = verificationCodeService;
    }

    /** 邮箱验证码有效时间（分钟） */
    @Value("${auth.server.email-code-live:5}")
    private long emailCodeLive;

    @Value("${spring.application.name:Auth-Server}")
    private String appName;

    @Value("${spring.mail.username}")
    private String fromUsername;

    @Value("${auth.server.login-page-url}")
    private String loginUrl;

    /**
     * 发送验证码邮件
     *
     * @param to 收件人
     */
    @Override
    public void sendEmailCode(String to) {
        // 1. 获取用户信息
        User user = userService.getOne(Wrappers.<User>lambdaQuery().select(User::getUsername).eq(User::getEmailAddress, to));
        if (user == null) {
            throw new BizException(MessageConstants.EMAIL_CODE_MSG_1000);
        }

        // 2. 设置验证码
        String code = verificationCodeService.setCode(to, emailCodeLive, ChronoUnit.MINUTES);

        // 3. 发送邮件
        MailInfo mailInfo = new MailInfo();
        mailInfo.setFrom(MessageFormat.format(DEFAULT_FROM_FORMAT, appName, fromUsername));
        mailInfo.setTo(List.of(to));
        mailInfo.setSubject(DEFAULT_EMAIL_CODE_SUBJECT);
        CompletableFuture.runAsync(() -> MailUtil.sendHtmlTemplateEmail(mailInfo, EMAIL_CODE_TEMPLATE_PATH,
                Map.of(CommonConstants.USERNAME, user.getUsername(), "emailCodeLive", emailCodeLive, "code", code)));
    }

    /**
     * 发送创建用户通知邮件
     *
     * @param to 收件人
     * @param username 用户名
     * @param password 初始密码
     */
    @Override
    public void sendCreateUserNotice(String to, String username, String password) {
        // 1. 发送邮件
        MailInfo mailInfo = new MailInfo();
        mailInfo.setFrom(MessageFormat.format(DEFAULT_FROM_FORMAT, appName, fromUsername));
        mailInfo.setTo(List.of(to));
        mailInfo.setSubject(DEFAULT_CREATE_USER_NOTICE_SUBJECT);
        CompletableFuture.runAsync(() -> MailUtil.sendHtmlTemplateEmail(mailInfo, CREATE_USER_NOTICE_PATH,
                Map.of(CommonConstants.USERNAME, username, "password", password, "loginUrl", loginUrl)));
    }

    /**
     * 发送重置密码通知邮件
     *
     * @param to 收件人
     * @param username 用户名
     * @param password 密码
     */
    @Override
    public void sendResetPwdNotice(String to, String username, String password) {
        // 1. 发送邮件
        MailInfo mailInfo = new MailInfo();
        mailInfo.setFrom(MessageFormat.format(DEFAULT_FROM_FORMAT, appName, fromUsername));
        mailInfo.setTo(List.of(to));
        mailInfo.setSubject(DEFAULT_RESET_PWD_NOTICE_SUBJECT);
        CompletableFuture.runAsync(() -> MailUtil.sendHtmlTemplateEmail(mailInfo, RESET_PWD_NOTICE_PATH,
                Map.of(CommonConstants.USERNAME, username, "password", password, "loginUrl", loginUrl)));
    }

    /**
     * 发送绑定邮箱验证码
     *
     * @param to 收件人
     */
    @Override
    public void sendBindEmailCode(String to) {
        // 1. 获取用户信息
        String userId = AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB);
        User user = userService.getById(userId);
        if (user == null) {
            throw new OAuth2AuthenticationException("invalid authentication");
        }

        // 2. 设置验证码
        String code = verificationCodeService.setCode(to, emailCodeLive, ChronoUnit.MINUTES);

        // 3. 发送邮件
        MailInfo mailInfo = new MailInfo();
        mailInfo.setFrom(MessageFormat.format(DEFAULT_FROM_FORMAT, appName, fromUsername));
        mailInfo.setTo(List.of(to));
        mailInfo.setSubject(DEFAULT_EMAIL_CODE_SUBJECT);
        CompletableFuture.runAsync(() -> MailUtil.sendHtmlTemplateEmail(mailInfo, BIND_EMAIL_TEMPLATE_PATH,
                Map.of(CommonConstants.USERNAME, user.getUsername(), "emailCodeLive", emailCodeLive, "code", code)));
    }

    @PostConstruct
    public void init() {
        MailUtil.setJavaMailSender(mailSender);
    }
}
