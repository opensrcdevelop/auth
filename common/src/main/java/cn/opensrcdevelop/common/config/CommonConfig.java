package cn.opensrcdevelop.common.config;

import cn.opensrcdevelop.common.exression.ExpressionEngine;
import cn.opensrcdevelop.common.exression.ICustomFunction;
import jakarta.validation.Validator;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ResourceBundleMessageSource;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;

import java.nio.charset.StandardCharsets;
import java.util.List;

@Configuration
public class CommonConfig {

    private static final String BEAN_NAME_COMMON_MSG_SRC = "commonMessageSource";

    /**
     * 公共国际化消息
     */
    @Bean(BEAN_NAME_COMMON_MSG_SRC)
    public MessageSource commonMessageSource() {
        ResourceBundleMessageSource messageSource = new ResourceBundleMessageSource();
        messageSource.setBasenames("i18n/common_messages");
        messageSource.setDefaultEncoding(StandardCharsets.UTF_8.name());
        return messageSource;
    }

    @Bean
    public Validator localValidator() {
        LocalValidatorFactoryBean localValidatorFactoryBean = new LocalValidatorFactoryBean();
        ResourceBundleMessageSource messageSource = new ResourceBundleMessageSource();
        messageSource.setBasenames("i18n/validation_messages");
        messageSource.setDefaultEncoding(StandardCharsets.UTF_8.name());
        localValidatorFactoryBean.setValidationMessageSource(messageSource);
        return localValidatorFactoryBean;
    }

    @Bean
    public ExpressionEngine expressionEngine(List<ICustomFunction> customFunctionList) {
        return new ExpressionEngine(1024, customFunctionList);
    }
}
