package cn.opensrcdevelop.auth.biz.service.system.impl;

import cn.opensrcdevelop.auth.biz.component.RotateJwtSecretApplicationRunner;
import cn.opensrcdevelop.auth.biz.component.ScheduledTaskService;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.SystemSettingConstants;
import cn.opensrcdevelop.auth.biz.dto.system.jwt.JwtSecretInfoDto;
import cn.opensrcdevelop.auth.biz.dto.system.jwt.JwtSecretRotationConfigDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailMessageConfigDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailServiceConfigDto;
import cn.opensrcdevelop.auth.biz.entity.system.SystemSetting;
import cn.opensrcdevelop.auth.biz.mapper.system.SystemSettingMapper;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.MailUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.tenant.support.TenantContext;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.RSAKey;
import io.vavr.control.Try;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.redisson.api.RLock;
import org.springframework.aop.framework.AopContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.charset.StandardCharsets;
import java.security.KeyPair;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

@Service
@RequiredArgsConstructor
public class SystemSettingServiceImpl extends ServiceImpl<SystemSettingMapper, SystemSetting> implements SystemSettingService {

    private static final Map<String, JavaMailSender> MAIL_SENDERS_CACHE = new ConcurrentHashMap<>();

    @Resource
    @Lazy
    private ScheduledTaskService scheduledTaskService;

    /**
     * 保存邮件服务配置
     *
     * @param mailServiceConfig 邮件服务配置
     */
    @Transactional
    @Override
    public void saveMailServerConfig(MailServiceConfigDto mailServiceConfig) {
        saveSystemSetting(SystemSettingConstants.MAIL_SERVER_CONFIG, mailServiceConfig);
        MailUtil.setJavaMailSender(buildJavaMailSender(mailServiceConfig));
    }

    /**
     * 获取邮件服务配置
     *
     * @return 邮件服务配置
     */
    @Override
    public MailServiceConfigDto getMailServerConfig() {
        return getSystemSetting(SystemSettingConstants.MAIL_SERVER_CONFIG, MailServiceConfigDto.class);
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
    @Transactional
    @Override
    public void saveMailMessageConfig(MailMessageConfigDto mailMessageConfig) {
        saveSystemSetting(SystemSettingConstants.MAIL_MESSAGE_CONFIG, mailMessageConfig);
    }

    /**
     * 获取邮件消息配置
     *
     * @return 邮件消息配置
     */
    @Override
    public MailMessageConfigDto getMailMessageConfig() {
        return getSystemSetting(SystemSettingConstants.MAIL_MESSAGE_CONFIG, MailMessageConfigDto.class);
    }

    /**
     * 获取 JWT 密钥信息
     *
     * @return JWT 密钥信息
     */
    @Override
    public JwtSecretInfoDto getJwtSecretInfo() {
        return getSystemSetting(SystemSettingConstants.JWT_SECRET_INFO, JwtSecretInfoDto.class);
    }

    /**
     * 获取 JWT 密钥轮换配置
     *
     * @return JWT 密钥轮换配置
     */
    @Override
    public JwtSecretRotationConfigDto getJwtSecretRotationConfig() {
        return getSystemSetting(SystemSettingConstants.JWT_SECRET_ROTATION_CONFIG, JwtSecretRotationConfigDto.class);
    }

    /**
     * 保存 JWT 密钥轮换配置
     *
     * @param jwtSecretRotationConfig JWT 密钥轮换配置
     */
    @Transactional
    @Override
    public void setJwtSecretRotationConfig(JwtSecretRotationConfigDto jwtSecretRotationConfig) {
        saveSystemSetting(SystemSettingConstants.JWT_SECRET_ROTATION_CONFIG, jwtSecretRotationConfig);
    }

    /**
     * 轮换 JWT 密钥
     *
     */
    @Transactional
    @Override
    public void rotateJwtSecret() {
        // 1. 获取 JWT 密钥轮换配置
        JwtSecretRotationConfigDto jwtSecretRotationConfig = getJwtSecretRotationConfig();
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        RLock lock = RedisUtil.getLock(LOCK_KEY_JWK + tenantCode);
        try {
            lock.lock();
            // 2. 删除 JWK 缓存
            RedisUtil.delete(AuthConstants.JWK_REDIS_KEY + tenantCode);

            // 3. 生成新的 JWK
            JwtSecretInfoDto jwtSecretInfo = getJwtSecretInfo();
            LocalDateTime createTime = LocalDateTime.now();
            String kid = CommonUtil.getBase64StringKey(18);
            jwtSecretInfo.setKid(kid);
            jwtSecretInfo.setAlg("RSA");
            jwtSecretInfo.setCreateTime(createTime);
            jwtSecretInfo.setExpireTime(createTime.plus(jwtSecretRotationConfig.getRotationPeriod(), CommonUtil.convertDBTimeUnit2ChronoUnit(jwtSecretRotationConfig.getRotationPeriodUnit())));
            saveSystemSetting(SystemSettingConstants.JWT_SECRET_INFO, jwtSecretInfo);

            KeyPair keyPair = CommonUtil.generateRsaKey();
            RSAPublicKey publicKey = (RSAPublicKey) keyPair.getPublic();
            RSAPrivateKey privateKey = (RSAPrivateKey) keyPair.getPrivate();
            RSAKey rsaKey = new RSAKey.Builder(publicKey)
                    .privateKey(privateKey)
                    .keyID(kid)
                    .build();
            RedisUtil.set(AuthConstants.JWK_REDIS_KEY + tenantCode, new JWKSet(rsaKey).toString(false));

            // 4. 重新添加轮换密钥任务
            // 4.1 取消已有的轮换密钥任务
            String taskName = RotateJwtSecretApplicationRunner.ROTATE_JWT_SECRET_TASK_NAME_PREFIX + tenantCode;
            scheduledTaskService.cancelTask(taskName);
            // 4.2 添加新的轮换密钥任务
            scheduledTaskService.addTaskAtFixedTime(taskName, () -> rotateJwtSecret(tenantCode), jwtSecretInfo.getExpireTime());
        } finally {
            lock.unlock();
        }
    }

    /**
     * 轮换 JWT 密钥
     *
     * @param tenantCode 租户
     */
    @Override
    public void rotateJwtSecret(String tenantCode) {
        try {
            // 1. 设置线程上下文
            TenantContext tenantContext = new TenantContext();
            tenantContext.setTenantCode(tenantCode);
            TenantContextHolder.setTenantContext(tenantContext);

            // 2. 切换租户数据源
            TenantHelper.switchTenantDs(tenantCode);

            // 3. 执行密钥轮换
            SystemSettingService proxy = (SystemSettingService) AopContext.currentProxy();
            proxy.rotateJwtSecret();
        } finally {
            // 4. 清除线程上下文
            TenantContextHolder.removeTenantContext();
        }
    }

    private <T> T getSystemSetting(String key, Class<T> clazz) {
        // 1. 从缓存获取
        SystemSetting systemSetting = RedisUtil.get(getCacheKey(key), SystemSetting.class);

        if (Objects.isNull(systemSetting)) {
            systemSetting = getByKey(key);
            // 2. 数据库操作
            if (Objects.nonNull(systemSetting)) {
                RedisUtil.set(getCacheKey(key), systemSetting);
            }
        }

        // 3. JSON 反序列化
        if (systemSetting != null) {
            return CommonUtil.deserializeObject(systemSetting.getValue(), clazz);
        }

        return Try.of(() -> clazz.getConstructor().newInstance()).getOrElseThrow(ServerException::new);
    }

    private <T> void saveSystemSetting(String key, T value) {
        // 1. JSON 序列化
        String configJson = CommonUtil.serializeObject(value);

        // 2. 删除缓存
        RedisUtil.delete(getCacheKey(key));

        // 3. 数据库操作
        SystemSetting systemSetting = new SystemSetting();
        systemSetting.setKey(key);
        systemSetting.setValue(configJson);
        super.saveOrUpdate(systemSetting);
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
