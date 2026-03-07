package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.ai.enums.ModelProviderType;
import cn.opensrcdevelop.ai.service.EmbeddingService;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import cn.opensrcdevelop.auth.biz.entity.system.SystemSetting;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import java.util.ArrayList;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.embedding.EmbeddingModel;
import org.springframework.ai.openai.OpenAiEmbeddingModel;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class EmbeddingServiceImpl implements EmbeddingService {

    private static final String EMBEDDING_PROVIDER_ID_KEY = "chatbi.embedding.provider.id";
    private static final String EMBEDDING_MODEL_KEY = "chatbi.embedding.model";
    private static final int DEFAULT_DIMENSION = 1536;

    private final ModelProviderService modelProviderService;
    private final SystemSettingService systemSettingService;

    @Override
    public List<Float> embedText(String text) {
        SystemSetting providerSetting = systemSettingService.getByKey(EMBEDDING_PROVIDER_ID_KEY);
        SystemSetting modelSetting = systemSettingService.getByKey(EMBEDDING_MODEL_KEY);

        // 去掉存储值外层的引号
        String providerId = stripQuotes(providerSetting != null ? providerSetting.getValue() : null);
        String embeddingModel = stripQuotes(modelSetting != null ? modelSetting.getValue() : null);

        if (providerId == null || providerId.isEmpty()) {
            log.warn("未配置嵌入模型提供商，跳过向量化");
            return new ArrayList<>();
        }

        try {
            // 获取模型提供商
            ModelProvider modelProvider = modelProviderService.getOne(Wrappers.<ModelProvider>lambdaQuery()
                    .eq(ModelProvider::getProviderId, providerId)
                    .eq(ModelProvider::getEnabled, true));

            if (modelProvider == null) {
                log.warn("嵌入模型提供商不存在或未启用: {}", providerId);
                return new ArrayList<>();
            }

            // 如果没有配置嵌入模型，使用默认的模型
            if (embeddingModel == null || embeddingModel.isEmpty()) {
                embeddingModel = modelProvider.getDefaultModel();
            }

            // 根据模型提供商类型创建嵌入模型
            ModelProviderType providerType = ModelProviderType.valueOf(modelProvider.getProviderType());
            float[] vector = createAndEmbed(providerType, modelProvider, embeddingModel, text);

            if (vector == null || vector.length == 0) {
                log.warn("嵌入模型返回空向量");
                return new ArrayList<>();
            }

            // 转换为 Float 列表
            List<Float> vectors = new ArrayList<>();
            for (float value : vector) {
                vectors.add(value);
            }

            log.debug("Text embedded successfully, dimension: {}", vectors.size());
            return vectors;
        } catch (Exception e) {
            log.error("获取嵌入向量失败", e);
            return new ArrayList<>();
        }
    }

    private float[] createAndEmbed(ModelProviderType providerType, ModelProvider modelProvider,
            String embeddingModel, String text) {
        return switch (providerType) {
            case OPENAI -> embedOpenAi(modelProvider, embeddingModel, text);
            case OLLAMA -> embedOllama(modelProvider, embeddingModel, text);
            case ANTHROPIC -> {
                // Anthropic 没有官方嵌入模型，这里可以抛出异常或使用其他方案
                throw new UnsupportedOperationException("Anthropic 暂不支持嵌入模型");
            }
        };
    }

    private float[] embedOpenAi(ModelProvider modelProvider, String embeddingModel, String text) {
        OpenAiApi openAiApi = OpenAiApi.builder()
                .baseUrl(modelProvider.getBaseUrl())
                .apiKey(modelProvider.getApiKey())
                .build();

        EmbeddingModel embeddingModelInstance = new OpenAiEmbeddingModel(openAiApi);

        // embed 方法直接返回 float[]
        return embeddingModelInstance.embed(text);
    }

    private float[] embedOllama(ModelProvider modelProvider, String embeddingModel, String text) {
        // Ollama 支持 OpenAI 兼容 API，可以使用 OpenAI 嵌入模型
        // Ollama 默认端口 11434，API 路径为 /v1/embeddings
        String baseUrl = modelProvider.getBaseUrl();
        // 如果 baseUrl 不包含 /v1，自动添加
        if (!baseUrl.contains("/v1")) {
            if (baseUrl.endsWith("/")) {
                baseUrl = baseUrl + "v1";
            } else {
                baseUrl = baseUrl + "/v1";
            }
        }

        return embedOpenAi(modelProvider, embeddingModel, text);
    }

    @Override
    public int getDimension() {
        // 实际应该从嵌入模型配置中获取维度
        // 这里简化处理，返回默认值
        return DEFAULT_DIMENSION;
    }

    private String stripQuotes(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        if (trimmed.startsWith("\"") && trimmed.endsWith("\"") && trimmed.length() >= 2) {
            return trimmed.substring(1, trimmed.length() - 1);
        }
        return trimmed;
    }
}
