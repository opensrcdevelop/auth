package cn.opensrcdevelop.common.util;

import cn.opensrcdevelop.common.entity.MailInfo;
import cn.opensrcdevelop.common.exception.ServerException;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import jakarta.mail.internet.MimeMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.IOUtils;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.Map;

@Slf4j
@SuppressWarnings("unused")
public class MailUtil {

    private MailUtil() {}

    private static JavaMailSender javaMailSender;

    public static synchronized void setJavaMailSender(JavaMailSender mailSender) {
        if (javaMailSender == null) {
            MailUtil.javaMailSender = mailSender;
        }
    }

    /**
     * 发送 Html 模板邮件
     *
     * @param mailInfo 邮件信息
     * @param htmlTemplatePath 模板路径
     * @param parameters 模板参数
     */
    public static void sendHtmlTemplateEmail(MailInfo mailInfo, String htmlTemplatePath, Map<String, Object> parameters) {
        try {
            // 1. 获取 Html 模板内容
            Resource resource = new ClassPathResource(htmlTemplatePath);
            String htmlTemplate = IOUtils.toString(resource.getInputStream(), StandardCharsets.UTF_8);

            // 2. 填充模板参数
            String content = fillTemplate(htmlTemplate, parameters);

            // 3. 发送邮件
            MimeMessage message = javaMailSender.createMimeMessage();
            MimeMessageHelper mimeMessageHelper = new MimeMessageHelper(message, true);
            mimeMessageHelper.setSubject(mailInfo.getSubject());
            mimeMessageHelper.setFrom(mailInfo.getFrom());
            mimeMessageHelper.setTo(mailInfo.getTo().toArray(new String[0]));
            mimeMessageHelper.setText(content, true);
            javaMailSender.send(message);
        } catch (Exception e) {
            log.error("邮件发送失败", e);
        }
    }

    private static String fillTemplate(String template, Map<String, Object> parameters) throws IOException, TemplateException {
        if (MapUtils.isEmpty(parameters)) {
            return template;
        }

        try (StringReader reader = new StringReader(template);
             StringWriter writer = new StringWriter()) {
            Template processor = new Template(CommonUtil.getUUIDString(), reader, null, StandardCharsets.UTF_8.name());
            processor.process(parameters, writer);
            return writer.toString();
        }
    }
}
