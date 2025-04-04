package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.system.mail.MailMessageConfigDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailServiceConfigDto;
import cn.opensrcdevelop.auth.biz.entity.SystemSetting;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.mail.javamail.JavaMailSender;

public interface SystemSettingService extends IService<SystemSetting> {

    void saveMailServerConfig(MailServiceConfigDto mailServiceConfig);

    MailServiceConfigDto getMailServerConfig();

    SystemSetting getByKey(String key);

    JavaMailSender buildJavaMailSender();

    void saveMailMessageConfig(MailMessageConfigDto mailMessageConfig);

    MailMessageConfigDto getMailMessageConfig();
}
