package cn.opensrcdevelop.ai.chat.advisor;

import cn.opensrcdevelop.ai.constants.SystemSettingConstants;
import cn.opensrcdevelop.ai.dto.ChatConfigDto;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import java.util.Map;
import org.springframework.ai.chat.client.ChatClientRequest;
import org.springframework.ai.chat.client.ChatClientResponse;
import org.springframework.ai.chat.client.advisor.api.AdvisorChain;
import org.springframework.ai.chat.client.advisor.api.BaseAdvisor;
import org.springframework.ai.chat.messages.SystemMessage;
import org.springframework.ai.chat.prompt.PromptTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.Ordered;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

@Component
public class LanguageConstraintAdvisor implements BaseAdvisor {

    private static final PromptTemplate SYSTEM_PROMPT_TEMPLATE = new PromptTemplate("""
            {instructions}
            ** MUST: Answer in language **: {lang}
            """);

    @Value("${ai.chat.language:简体中文}")
    private String defaultLanguage;

    private final SystemSettingService systemSettingService;

    public LanguageConstraintAdvisor(SystemSettingService systemSettingService) {
        this.systemSettingService = systemSettingService;
    }

    @Override
    @NonNull
    public ChatClientRequest before(@NonNull ChatClientRequest chatClientRequest, @NonNull AdvisorChain advisorChain) {
        // 从系统设置获取语言配置
        String language = defaultLanguage;
        try {
            ChatConfigDto config = systemSettingService.getSystemSetting(
                    SystemSettingConstants.CHATBI_CHAT_CONFIG, ChatConfigDto.class);
            if (config != null && config.getLanguage() != null) {
                language = config.getLanguage();
            }
        } catch (Exception e) {
            // 使用默认值
        }

        SystemMessage systemMessage = chatClientRequest.prompt().getSystemMessage();
        String augmentedSystemText = SYSTEM_PROMPT_TEMPLATE
                .render(Map.of("instructions", systemMessage.getText(), "lang", language));

        return chatClientRequest.mutate()
                .prompt(chatClientRequest.prompt().augmentSystemMessage(augmentedSystemText))
                .build();
    }

    @Override
    @NonNull
    public ChatClientResponse after(@NonNull ChatClientResponse chatClientResponse,
            @NonNull AdvisorChain advisorChain) {
        return chatClientResponse;
    }

    @Override
    public int getOrder() {
        return Ordered.HIGHEST_PRECEDENCE;
    }
}
