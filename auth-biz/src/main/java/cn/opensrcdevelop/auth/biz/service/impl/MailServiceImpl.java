package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.entity.MailTemplate;
import cn.opensrcdevelop.auth.biz.entity.PasswordPolicy;
import cn.opensrcdevelop.auth.biz.entity.UpdatePasswordRemindLog;
import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.service.*;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.entity.MailInfo;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.MailUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
public class MailServiceImpl implements MailService {

    private static final String PROP_CONSOLE_REDIRECT_PATH = "auth.server.console-redirect-path";
    private static final String MAIL_VERIFY_TEMPLATE = "mail_verify";
    private static final String CREATE_USER_NOTICE_TEMPLATE = "create_user";
    private static final String RESET_PWD_NOTICE_TEMPLATE = "reset_password";
    private static final String BIND_EMAIL_TEMPLATE = "bind_email";
    private static final String REMIND_UPDATE_PWD_TEMPLATE = "remind_update_password";


    @Resource
    @Lazy
    private UserService userService;

    private final VerificationCodeService verificationCodeService;
    private final MailTemplateService mailTemplateService;
    private final SystemSettingService systemSettingService;
    private final UpdatePasswordRemindLogService updatePasswordRemindLogService;

    /**
     * 发送邮箱验证码
     *
     * @param to 收件人
     */
    @Override
    public void sendMailCode(String to) {
        // 1. 获取用户信息
        User user = userService.getOne(Wrappers.<User>lambdaQuery().select(User::getUsername).eq(User::getEmailAddress, to));
        if (user == null) {
            throw new BizException(MessageConstants.EMAIL_CODE_MSG_1000);
        }

        // 2. 设置验证码
        Integer codeLive = systemSettingService.getMailMessageConfig().getCodeLive();
        String code = verificationCodeService.setCode(to, codeLive, ChronoUnit.MINUTES);

        // 3. 发送邮件
        // 3.1 获取邮件模版
        MailTemplate mailTemplate = mailTemplateService.getByCode(MAIL_VERIFY_TEMPLATE);

        // 3.2 参数设置
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("code", code);
        parameters.put("code_live", codeLive);
        parameters.put(CommonConstants.USERNAME, user.getUsername());

        // 3.3 设置 JavaMailSender
        MailUtil.setJavaMailSender(systemSettingService.buildJavaMailSender());

        // 3.4 发送邮件
        MailInfo mailInfo = new MailInfo();
        mailInfo.setFrom(mailTemplate.getSender());
        mailInfo.setTo(List.of(to));
        mailInfo.setSubject(mailTemplate.getSubject());
        mailInfo.setContent(mailTemplate.getTemplateContent());
        CompletableFuture.runAsync(() -> MailUtil.sendHtmlTemplateEmail(mailInfo, parameters));
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
        // 1.1 获取邮件模版
        MailTemplate mailTemplate = mailTemplateService.getByCode(CREATE_USER_NOTICE_TEMPLATE);

        // 1.2 参数设置
        Map<String, Object> parameters = new HashMap<>();
        parameters.put(CommonConstants.USERNAME, username);
        parameters.put("password", password);
        parameters.put("login_url", WebUtil.getRootUrl() + SpringContextUtil.getProperty(PROP_CONSOLE_REDIRECT_PATH));

        // 1.3 设置 JavaMailSender
        MailUtil.setJavaMailSender(systemSettingService.buildJavaMailSender());

        // 1.4 发送邮件
        MailInfo mailInfo = new MailInfo();
        mailInfo.setFrom(mailTemplate.getSender());
        mailInfo.setTo(List.of(to));
        mailInfo.setSubject(mailTemplate.getSubject());
        mailInfo.setContent(mailTemplate.getTemplateContent());
        CompletableFuture.runAsync(() -> MailUtil.sendHtmlTemplateEmail(mailInfo, parameters));
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
        // 1.1 获取邮件模版
        MailTemplate mailTemplate = mailTemplateService.getByCode(RESET_PWD_NOTICE_TEMPLATE);

        // 1.2 参数设置
        Map<String, Object> parameters = new HashMap<>();
        parameters.put(CommonConstants.USERNAME, username);
        parameters.put("password", password);
        parameters.put("login_url", WebUtil.getRootUrl() + SpringContextUtil.getProperty(PROP_CONSOLE_REDIRECT_PATH));

        // 1.3 设置 JavaMailSender
        MailUtil.setJavaMailSender(systemSettingService.buildJavaMailSender());

        // 1.4 发送邮件
        MailInfo mailInfo = new MailInfo();
        mailInfo.setFrom(mailTemplate.getSender());
        mailInfo.setTo(List.of(to));
        mailInfo.setSubject(mailTemplate.getSubject());
        mailInfo.setContent(mailTemplate.getTemplateContent());
        CompletableFuture.runAsync(() -> MailUtil.sendHtmlTemplateEmail(mailInfo, parameters));
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
        Integer codeLive = systemSettingService.getMailMessageConfig().getCodeLive();
        String code = verificationCodeService.setCode(to, codeLive, ChronoUnit.MINUTES);

        // 3. 发送邮件
        // 3.1 获取邮件模版
        MailTemplate mailTemplate = mailTemplateService.getByCode(BIND_EMAIL_TEMPLATE);

        // 3.2 参数设置
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("code", code);
        parameters.put("code_live", codeLive);
        parameters.put(CommonConstants.USERNAME, user.getUsername());

        // 3.3 设置 JavaMailSender
        MailUtil.setJavaMailSender(systemSettingService.buildJavaMailSender());

        // 3.4 发送邮件
        MailInfo mailInfo = new MailInfo();
        mailInfo.setFrom(mailTemplate.getSender());
        mailInfo.setTo(List.of(to));
        mailInfo.setSubject(mailTemplate.getSubject());
        mailInfo.setContent(mailTemplate.getTemplateContent());
        CompletableFuture.runAsync(() -> MailUtil.sendHtmlTemplateEmail(mailInfo, parameters));
    }

    /**
     * 发送修改密码提醒邮件
     *
     * @param user 用户
     * @param expireTime 过期时间
     * @param executeTime 执行时间
     */
    @Override
    public void sendRemindUpdatePwd(User user, PasswordPolicy passwordPolicy, LocalDateTime expireTime, LocalDateTime executeTime) {
        // 1. 发送邮件
        // 1.1 获取邮件模版
        MailTemplate mailTemplate = mailTemplateService.getByCode(REMIND_UPDATE_PWD_TEMPLATE);

        // 1.2 参数设置
        Map<String, Object> parameters = new HashMap<>();
        parameters.put(CommonConstants.USERNAME, user.getUsername());
        parameters.put("expire_time", expireTime.format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)));

        // 1.3 设置 JavaMailSender
        MailUtil.setJavaMailSender(systemSettingService.buildJavaMailSender());

        // 1.4 发送邮件
        MailInfo mailInfo = new MailInfo();
        mailInfo.setFrom(mailTemplate.getSender());
        mailInfo.setTo(List.of(user.getEmailAddress()));
        mailInfo.setSubject(mailTemplate.getSubject());
        mailInfo.setContent(mailTemplate.getTemplateContent());
        CompletableFuture.runAsync(() -> MailUtil.sendHtmlTemplateEmail(mailInfo, parameters))
                .whenComplete((result, ex) -> {
                    // 1.5 记录日志
                    boolean success = ex == null;
                    UpdatePasswordRemindLog remindLog = new UpdatePasswordRemindLog();
                    remindLog.setUserId(user.getUserId());
                    remindLog.setPolicyId(passwordPolicy.getPolicyId());
                    remindLog.setRemindTime(executeTime);
                    remindLog.setRemindMethod("MAIL");
                    remindLog.setSuccess(success);

                    updatePasswordRemindLogService.save(remindLog);
                });
    }
}
