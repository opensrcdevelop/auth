package cn.opensrcdevelop.ai.chat.client;

import cn.opensrcdevelop.ai.chat.advisor.LanguageConstraintAdvisor;
import cn.opensrcdevelop.ai.chat.advisor.TokenCountAdvisor;
import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.constants.SystemSettingConstants;
import cn.opensrcdevelop.ai.dto.ChatConfigDto;
import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.ai.enums.ModelProviderType;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.anthropic.AnthropicChatModel;
import org.springframework.ai.anthropic.AnthropicChatOptions;
import org.springframework.ai.anthropic.api.AnthropicApi;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.client.advisor.SimpleLoggerAdvisor;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.model.tool.ToolCallingManager;
import org.springframework.ai.ollama.OllamaChatModel;
import org.springframework.ai.ollama.api.OllamaApi;
import org.springframework.ai.ollama.api.OllamaChatOptions;
import org.springframework.ai.openai.OpenAiChatModel;
import org.springframework.ai.openai.OpenAiChatOptions;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.util.Objects;

@Slf4j
@Component
@RequiredArgsConstructor
public class ChatClientManager {

    private final ToolCallingManager toolCallingManager;
    private final ModelProviderService modelProviderService;
    private final LanguageConstraintAdvisor languageConstraintAdvisor;
    private final TokenCountAdvisor tokenCountAdvisor;
    private final SystemSettingService systemSettingService;

    @Value("${ai.chat.default-llm-api-retry-count:3}")
    private Integer defaultLlmApiRetryCount;

    @Value("${ai.chat.default-temperature:0.0}")
    private Double defaultTemperature;

    /**
     * 获取 ChatClient
     *
     * @param providerId
     *            模型提供商ID
     * @param model
     *            模型名称
     * @param chatId
     *            对话ID
     * @return ChatClient
     */
    public synchronized ChatClient getChatClient(String providerId, String model, String chatId) {
        // 1. 获取模型提供商
        ModelProvider modelProvider = modelProviderService
                .getOne(Wrappers.<ModelProvider>lambdaQuery()
                        .eq(ModelProvider::getProviderId, providerId)
                        .eq(ModelProvider::getEnabled, true));
        if (Objects.isNull(modelProvider)) {
            throw new BizException(MessageConstants.AI_MODEL_MSG_1000, providerId);
        }

        // 2. 获取对话配置
        ChatConfigDto chatConfig = null;
        try {
            chatConfig = systemSettingService.getSystemSetting(SystemSettingConstants.CHATBI_CHAT_CONFIG,
                    ChatConfigDto.class);
        } catch (Exception e) {
            log.warn("获取 ChatBI 对话配置失败", e);
        }

        // 3. 根据模型提供商类型创建 ChatModel
        ModelProviderType modelProviderType = ModelProviderType.valueOf(modelProvider.getProviderType());
        ChatModel chatModel = switch (modelProviderType) {
            case OPENAI -> createOpenAiChatModel(modelProvider, model, chatConfig);
            case ANTHROPIC -> createAnthropicChatModel(modelProvider, model, chatConfig);
            case OLLAMA -> createOllamaChatModel(modelProvider, model, chatConfig);
        };

        // 4. 返回 ChatClient
        ChatClient.Builder builder = ChatClient.builder(chatModel)
                .defaultAdvisors(a -> a
                        .param(ChatMemory.CONVERSATION_ID, chatId)
                        .param("model_provider_id", providerId)
                        .param("model", model))
                .defaultAdvisors(languageConstraintAdvisor, tokenCountAdvisor,
                        SimpleLoggerAdvisor.builder().requestToString(CommonUtil::formatJson).build());
        return builder.build();
    }

    private ChatModel createOpenAiChatModel(ModelProvider modelProvider, String model, ChatConfigDto chatConfig) {
        Double temperature = defaultTemperature;
        if (Objects.nonNull(chatConfig) && Objects.nonNull(chatConfig.getTemperature())) {
            temperature = chatConfig.getTemperature();
        }

        return OpenAiChatModel.builder()
                .openAiApi(OpenAiApi.builder()
                        .baseUrl(modelProvider.getBaseUrl())
                        .apiKey(modelProvider.getApiKey())
                        .build())
                .defaultOptions(OpenAiChatOptions.builder()
                        .model(StringUtils.isEmpty(model) ? modelProvider.getDefaultModel() : model)
                        .temperature(temperature)
                        .build())
                .toolCallingManager(toolCallingManager)
                .retryTemplate(getRetryTemplate(chatConfig))
                .build();
    }

    private ChatModel createOllamaChatModel(ModelProvider modelProvider, String model, ChatConfigDto chatConfig) {
        Double temperature = defaultTemperature;
        if (Objects.nonNull(chatConfig) && Objects.nonNull(chatConfig.getTemperature())) {
            temperature = chatConfig.getTemperature();
        }

        return OllamaChatModel.builder()
                .ollamaApi(OllamaApi.builder()
                        .baseUrl(modelProvider.getBaseUrl())
                        .build())
                .defaultOptions(OllamaChatOptions.builder()
                        .model(StringUtils.isEmpty(model) ? modelProvider.getDefaultModel() : model)
                        .temperature(temperature)
                        .build())
                .toolCallingManager(toolCallingManager)
                .build();
    }

    private ChatModel createAnthropicChatModel(ModelProvider modelProvider, String model, ChatConfigDto chatConfig) {
        Double temperature = defaultTemperature;
        if (Objects.nonNull(chatConfig) && Objects.nonNull(chatConfig.getTemperature())) {
            temperature = chatConfig.getTemperature();
        }

        return AnthropicChatModel.builder()
                .anthropicApi(AnthropicApi.builder()
                        .baseUrl(modelProvider.getBaseUrl())
                        .apiKey(modelProvider.getApiKey())
                        .build())
                .defaultOptions(AnthropicChatOptions.builder()
                        .model(StringUtils.isEmpty(model) ? modelProvider.getDefaultModel() : model)
                        .temperature(temperature)
                        .build())
                .toolCallingManager(toolCallingManager)
                .retryTemplate(getRetryTemplate(chatConfig))
                .build();
    }

    private RetryTemplate getRetryTemplate(ChatConfigDto chatConfig) {
        Integer retryCount = defaultLlmApiRetryCount;
        if (Objects.nonNull(chatConfig) && Objects.nonNull(retryCount) && retryCount > 0) {
            retryCount = chatConfig.getLlmApiRetryCount();
        }

        return RetryTemplate.builder()
                .maxAttempts(retryCount)
                .exponentialBackoff(Duration.ofSeconds(1), 2, Duration.ofMinutes(1))
                .build();
    }
}
