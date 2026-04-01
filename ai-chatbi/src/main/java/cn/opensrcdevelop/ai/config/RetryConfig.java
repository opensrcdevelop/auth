package cn.opensrcdevelop.ai.config;

import cn.opensrcdevelop.ai.constants.SystemSettingConstants;
import cn.opensrcdevelop.ai.dto.ChatConfigDto;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.retry.backoff.ExponentialBackOffPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.retry.support.RetryTemplate;

/**
 * ChatBI 重试配置
 */
@Configuration
public class RetryConfig {

    private final SystemSettingService systemSettingService;

    public RetryConfig(SystemSettingService systemSettingService) {
        this.systemSettingService = systemSettingService;
    }

    @Bean
    @ConditionalOnMissingBean
    public RetryTemplate retryTemplate() {
        // 从系统设置获取重试次数配置
        int retryCount = 3;
        try {
            ChatConfigDto config = systemSettingService.getSystemSetting(
                    SystemSettingConstants.CHATBI_CHAT_CONFIG, ChatConfigDto.class);
            if (config != null && config.getApiRetryCount() != null) {
                retryCount = config.getApiRetryCount();
            }
        } catch (Exception e) {
            // 使用默认值
        }

        RetryTemplate retryTemplate = new RetryTemplate();

        // 设置重试策略
        SimpleRetryPolicy retryPolicy = new SimpleRetryPolicy();
        retryPolicy.setMaxAttempts(retryCount + 1); // 重试次数 + 1 次初始调用
        retryTemplate.setRetryPolicy(retryPolicy);

        // 设置退避策略（指数退避）
        ExponentialBackOffPolicy backOffPolicy = new ExponentialBackOffPolicy();
        backOffPolicy.setInitialInterval(1000); // 初始间隔 1 秒
        backOffPolicy.setMultiplier(2.0); // 倍数 2
        backOffPolicy.setMaxInterval(10000); // 最大间隔 10 秒
        retryTemplate.setBackOffPolicy(backOffPolicy);

        return retryTemplate;
    }
}
