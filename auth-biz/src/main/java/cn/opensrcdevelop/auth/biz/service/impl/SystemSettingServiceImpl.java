package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.SystemSettingConstants;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailMessageConfigDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailServiceConfigDto;
import cn.opensrcdevelop.auth.biz.entity.SystemSetting;
import cn.opensrcdevelop.auth.biz.mapper.SystemSettingMapper;
import cn.opensrcdevelop.auth.biz.service.SystemSettingService;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.MailUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

@Service
public class SystemSettingServiceImpl extends ServiceImpl<SystemSettingMapper, SystemSetting> implements SystemSettingService {

    private static final Map<String, JavaMailSender> MAIL_SENDERS_CACHE = new ConcurrentHashMap<>();

    /**
     * 保存邮件服务配置
     *
     * @param mailServiceConfig 邮件服务配置
     */
    @Transactional
    @Override
    public void saveMailServerConfig(MailServiceConfigDto mailServiceConfig) {
        // 1. JSON 序列化
        String configJson = CommonUtil.serializeObject(mailServiceConfig);

        // 2. 删除缓存
        RedisUtil.delete(getCacheKey(SystemSettingConstants.MAIL_SERVER_CONFIG));
        MAIL_SENDERS_CACHE.remove(TenantContextHolder.getTenantContext().getTenantCode());

        // 3. 数据库操作
        SystemSetting systemSetting = new SystemSetting();
        systemSetting.setKey(SystemSettingConstants.MAIL_SERVER_CONFIG);
        systemSetting.setValue(configJson);
        super.saveOrUpdate(systemSetting);

        // 4. JavaMailSender 配置
        MailUtil.setJavaMailSender(buildJavaMailSender(mailServiceConfig));
    }

    /**
     * 获取邮件服务配置
     *
     * @return 邮件服务配置
     */
    @Override
    public MailServiceConfigDto getMailServerConfig() {
        // 1. 从缓存获取
        SystemSetting systemSetting = RedisUtil.get(getCacheKey(SystemSettingConstants.MAIL_SERVER_CONFIG), SystemSetting.class);

        // 2. 数据库操作
        if (Objects.isNull(systemSetting)) {
            systemSetting = getByKey(SystemSettingConstants.MAIL_SERVER_CONFIG);
            if (systemSetting != null) {
                RedisUtil.set(getCacheKey(SystemSettingConstants.MAIL_SERVER_CONFIG), systemSetting);
            }
        }

        // 3. JSON 反序列化
        if (systemSetting != null) {
            return CommonUtil.deserializeObject(systemSetting.getValue(), MailServiceConfigDto.class);
        }
        return new MailServiceConfigDto();
    }

    /**
     * 根据 key 获取系统设置
     *
     * @param key key
     * @return 系统配置
     */
    @Override
    public SystemSetting getByKey(String key) {
        return super.getOne(Wrappers.<SystemSetting>lambdaQuery().eq(SystemSetting::getKey, key));
    }

    /**
     * 构建 JavaMailSender
     *
     * @return JavaMailSender
     */
    @Override
    public JavaMailSender buildJavaMailSender() {
        return buildJavaMailSender(getMailServerConfig());
    }

    /**
     * 保存邮件消息配置
     *
     * @param mailMessageConfig 邮件消息配置
     */
    @Override
    public void saveMailMessageConfig(MailMessageConfigDto mailMessageConfig) {
        // 1. JSON 序列化
        String configJson = CommonUtil.serializeObject(mailMessageConfig);

        // 2. 删除缓存
        RedisUtil.delete(getCacheKey(SystemSettingConstants.MAIL_MESSAGE_CONFIG));

        // 3. 数据库操作
        SystemSetting systemSetting = new SystemSetting();
        systemSetting.setKey(SystemSettingConstants.MAIL_MESSAGE_CONFIG);
        systemSetting.setValue(configJson);
        super.saveOrUpdate(systemSetting);
    }

    /**
     * 获取邮件消息配置
     *
     * @return 邮件消息配置
     */
    @Override
    public MailMessageConfigDto getMailMessageConfig() {
        // 1. 从缓存获取
        SystemSetting systemSetting = RedisUtil.get(getCacheKey(SystemSettingConstants.MAIL_MESSAGE_CONFIG), SystemSetting.class);

        // 2. 数据库操作
        if (Objects.isNull(systemSetting)) {
            systemSetting = getByKey(SystemSettingConstants.MAIL_MESSAGE_CONFIG);
            if (systemSetting != null) {
                RedisUtil.set(getCacheKey(SystemSettingConstants.MAIL_MESSAGE_CONFIG), systemSetting);
            }
        }

        // 3. JSON 反序列化
        if (systemSetting != null) {
            return CommonUtil.deserializeObject(systemSetting.getValue(), MailMessageConfigDto.class);
        }
        return new MailMessageConfigDto();
    }

    private String getCacheKey(String key) {
        return CacheConstants.CACHE_SYSTEM_SETTING
                + ":"
                + TenantContextHolder.getTenantContext().getTenantCode()
                + ":"
                + key;
    }

    private JavaMailSender buildJavaMailSender(MailServiceConfigDto mailServiceConfig) {
        // 1. 从缓存获取
        JavaMailSender javaMailSender = MAIL_SENDERS_CACHE.get(TenantContextHolder.getTenantContext().getTenantCode());

        if (Objects.nonNull(javaMailSender)) {
            return javaMailSender;
        }

        // 2. 构建 JavaMailSender
        JavaMailSenderImpl mailSender = new JavaMailSenderImpl();
        mailSender.setHost(mailServiceConfig.getHost());
        mailSender.setPort(mailServiceConfig.getPort());
        mailSender.setUsername(mailServiceConfig.getUsername());
        mailSender.setPassword(mailServiceConfig.getPassword());
        mailSender.setDefaultEncoding(StandardCharsets.UTF_8.displayName());

        Properties mailProperties = new Properties();
        if (Boolean.TRUE.equals(mailServiceConfig.getSslEnable())) {
            mailProperties.setProperty("mail.smtp.auth", "true");
            mailProperties.setProperty("mail.smtp.starttls.enable", "true");
            mailProperties.setProperty("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory");
        }
        mailSender.setJavaMailProperties(mailProperties);

        // 3. 缓存 JavaMailSender
        MAIL_SENDERS_CACHE.put(TenantContextHolder.getTenantContext().getTenantCode(), mailSender);
        return mailSender;
    }
}
