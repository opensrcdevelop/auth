package cn.opensrcdevelop.auth.biz.service.system;

import cn.opensrcdevelop.auth.biz.dto.system.jwt.JwtSecretInfoDto;
import cn.opensrcdevelop.auth.biz.dto.system.jwt.JwtSecretRotationConfigDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailMessageConfigDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailServiceConfigDto;
import cn.opensrcdevelop.auth.biz.entity.system.SystemSetting;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.mail.javamail.JavaMailSender;

public interface SystemSettingService extends IService<SystemSetting> {

    String LOCK_KEY_JWK = "lock_jwk:";

    void saveMailServerConfig(MailServiceConfigDto mailServiceConfig);

    MailServiceConfigDto getMailServerConfig();

    SystemSetting getByKey(String key);

    JavaMailSender buildJavaMailSender();

    void saveMailMessageConfig(MailMessageConfigDto mailMessageConfig);

    MailMessageConfigDto getMailMessageConfig();

    JwtSecretInfoDto getJwtSecretInfo();

    JwtSecretRotationConfigDto getJwtSecretRotationConfig();

    void setJwtSecretRotationConfig(JwtSecretRotationConfigDto jwtSecretRotationConfig);

    void rotateJwtSecret();

    void rotateJwtSecret(String tenantCode);
}
