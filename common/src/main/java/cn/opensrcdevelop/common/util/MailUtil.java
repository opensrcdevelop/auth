package cn.opensrcdevelop.common.util;

import cn.opensrcdevelop.common.entity.MailInfo;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import jakarta.mail.internet.MimeMessage;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;

@Slf4j
@SuppressWarnings("unused")
public class MailUtil {

    private MailUtil() {
    }

    private static final ReentrantLock LOCK = new ReentrantLock();
    private static JavaMailSender javaMailSender;

    public static void setJavaMailSender(JavaMailSender mailSender) {
        LOCK.lock();
        try {
            if (javaMailSender == null) {
                javaMailSender = mailSender;
            }
        } finally {
            LOCK.unlock();
        }
    }

    /**
     * 发送 Html 模板邮件
     *
     * @param mailInfo
     *            邮件信息
     * @param parameters
     *            模板参数
     */
    public static void sendHtmlTemplateEmail(MailInfo mailInfo, Map<String, Object> parameters) {
        log.info("发送邮件入参: 发送者: [{}] 接收者: [{}]", mailInfo.getFrom(), mailInfo.getTo());
        try {
            // 1. 填充模板参数
            parameters.put("sender", ((JavaMailSenderImpl) javaMailSender).getUsername());
            String content = fillTemplate(mailInfo.getContent(), parameters);
            String subject = fillTemplate(mailInfo.getSubject(), parameters);
            String from = fillTemplate(mailInfo.getFrom(), parameters);

            // 2. 发送邮件
            MimeMessage message = javaMailSender.createMimeMessage();
            MimeMessageHelper mimeMessageHelper = new MimeMessageHelper(message, true);
            mimeMessageHelper.setSubject(subject);
            mimeMessageHelper.setFrom(from);
            mimeMessageHelper.setTo(mailInfo.getTo().toArray(new String[0]));
            mimeMessageHelper.setText(content, true);
            mimeMessageHelper.setSentDate(new Date());
            javaMailSender.send(message);
        } catch (Exception e) {
            log.error("邮件发送失败", e);
        }
    }

    private static String fillTemplate(String template, Map<String, Object> parameters)
            throws IOException, TemplateException {
        if (MapUtils.isEmpty(parameters)) {
            return template;
        }

        try (StringReader reader = new StringReader(template);
                StringWriter writer = new StringWriter()) {
            Template processor = new Template(CommonUtil.getUUIDV7String(), reader, null,
                    StandardCharsets.UTF_8.name());
            processor.process(parameters, writer);
            return writer.toString();
        }
    }
}
