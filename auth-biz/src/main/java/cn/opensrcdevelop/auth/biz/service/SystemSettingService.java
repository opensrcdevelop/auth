package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.MailMessageConfigDto;
import cn.opensrcdevelop.auth.biz.dto.MailServiceConfigDto;
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
