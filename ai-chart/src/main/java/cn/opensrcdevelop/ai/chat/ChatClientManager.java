package cn.opensrcdevelop.ai.chat;

import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.ai.enums.ModelProviderType;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import cn.opensrcdevelop.common.exception.BizException;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.anthropic.AnthropicChatModel;
import org.springframework.ai.anthropic.AnthropicChatOptions;
import org.springframework.ai.anthropic.api.AnthropicApi;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.chat.prompt.ChatOptions;
import org.springframework.ai.model.tool.ToolCallingManager;
import org.springframework.ai.ollama.OllamaChatModel;
import org.springframework.ai.ollama.api.OllamaApi;
import org.springframework.ai.ollama.api.OllamaOptions;
import org.springframework.ai.openai.OpenAiChatModel;
import org.springframework.ai.openai.OpenAiChatOptions;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Component;

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

@Component
@RequiredArgsConstructor
public class ChatClientManager {
    private static final ConcurrentHashMap<String, ChatModel> CHAT_MODEL_CACHE = new ConcurrentHashMap<>();

    private final RetryTemplate retryTemplate;
    private final ToolCallingManager toolCallingManager;
    private final ModelProviderService modelProviderService;
    private final ChatMemory chatMemory;
    private final LanguageConstraintAdvisor languageConstraintAdvisor;

    /**
     * 获取 ChatClient
     *
     * @param providerId 模型提供商ID
     * @param model 模型名称
     * @param chatId 对话ID
     * @return ChatClient
     */
    public synchronized ChatClient getChatClient(String providerId, String model, String chatId) {
        // 1. 检查缓存中是否已经存在该模型
        ChatModel chatModel = CHAT_MODEL_CACHE.get(providerId);

        // 2. 获取模型提供商
        if (Objects.isNull(chatModel)) {
            ModelProvider modelProvider = modelProviderService
                    .getOne(Wrappers.<ModelProvider>lambdaQuery()
                            .eq(ModelProvider::getProviderId, providerId)
                            .eq(ModelProvider::getEnabled, true));
            if (Objects.isNull(modelProvider)) {
                throw new BizException(MessageConstants.AI_MODEL_MSG_1000, providerId);
            }
            // 3. 根据模型提供商类型创建 ChatModel
            ModelProviderType modelProviderType = ModelProviderType.valueOf(modelProvider.getProviderType());
            chatModel = switch (modelProviderType) {
                case OPENAI -> createOpenAiChatModel(modelProvider);
                case ANTHROPIC -> createAnthropicChatModel(modelProvider);
                case OLLAMA -> createOllamaChatModel(modelProvider);
            };
            CHAT_MODEL_CACHE.put(providerId, chatModel);
        }

        // 4. 返回 ChatClient
        return ChatClient.builder(chatModel)
                .defaultOptions(ChatOptions.builder().model(model).build())
                .defaultAdvisors(a -> a.param(ChatMemory.CONVERSATION_ID, chatId))
                .defaultAdvisors(languageConstraintAdvisor)
                .build();
    }

    private ChatModel createOpenAiChatModel(ModelProvider modelProvider) {
        return OpenAiChatModel.builder()
                .openAiApi(OpenAiApi.builder()
                        .baseUrl(modelProvider.getBaseUrl())
                        .apiKey(modelProvider.getApiKey())
                        .build())
                .defaultOptions(OpenAiChatOptions.builder()
                        .model(modelProvider.getDefaultModel())
                        .temperature(modelProvider.getTemperature())
                        .maxTokens(modelProvider.getMaxTokens())
                        .build())
                .toolCallingManager(toolCallingManager)
                .retryTemplate(retryTemplate)
                .build();
    }

    private ChatModel createOllamaChatModel(ModelProvider modelProvider) {
        return OllamaChatModel.builder()
                .ollamaApi(OllamaApi.builder()
                        .baseUrl(modelProvider.getBaseUrl())
                        .build())
                .defaultOptions(OllamaOptions.builder()
                        .model(modelProvider.getDefaultModel())
                        .temperature(modelProvider.getTemperature())
                        .build())
                .toolCallingManager(toolCallingManager)
                .build();
    }

    private ChatModel createAnthropicChatModel(ModelProvider modelProvider) {
        return AnthropicChatModel.builder()
                .anthropicApi(AnthropicApi.builder()
                        .baseUrl(modelProvider.getBaseUrl())
                        .apiKey(modelProvider.getApiKey())
                        .build())
                .defaultOptions(AnthropicChatOptions.builder()
                        .model(modelProvider.getDefaultModel())
                        .temperature(modelProvider.getTemperature())
                        .maxTokens(modelProvider.getMaxTokens())
                        .build())
                .toolCallingManager(toolCallingManager)
                .retryTemplate(retryTemplate)
                .build();
    }
}
