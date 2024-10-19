package cn.opensrcdevelop.auth.config;

import com.anji.captcha.config.AjCaptchaServiceAutoConfiguration;
import com.anji.captcha.properties.AjCaptchaProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

@Configuration
@EnableConfigurationProperties(AjCaptchaProperties.class)
@Import(AjCaptchaServiceAutoConfiguration.class)
public class CaptchaConfig {
}
