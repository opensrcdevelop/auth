package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.constants.SystemSettingConstants;
import cn.opensrcdevelop.ai.dto.SampleSqlEmbeddingConfigDto;
import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.ai.enums.ModelProviderType;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import cn.opensrcdevelop.ai.service.SampleSqlEmbeddingService;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.exception.BizException;
import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.embedding.EmbeddingModel;
import org.springframework.ai.ollama.OllamaEmbeddingModel;
import org.springframework.ai.ollama.api.OllamaApi;
import org.springframework.ai.ollama.api.OllamaEmbeddingOptions;
import org.springframework.ai.openai.OpenAiEmbeddingModel;
import org.springframework.ai.openai.OpenAiEmbeddingOptions;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Slf4j
@Service
@RequiredArgsConstructor
public class SampleSqlEmbeddingServiceImpl implements SampleSqlEmbeddingService {
    private final ModelProviderService modelProviderService;
    private final SystemSettingService systemSettingService;

    @Override
    public List<Float> embedText(String text) {
        SampleSqlEmbeddingConfigDto embeddingConfig = systemSettingService.getSystemSetting(
                SystemSettingConstants.SAMPLE_SQL_EMBEDDING_CONFIG, SampleSqlEmbeddingConfigDto.class);

        if (Objects.isNull(embeddingConfig)) {
            throw new BizException(MessageConstants.AI_SAMPLE_SQL_MSG_1000);
        }

        try {
            ModelProvider modelProvider = modelProviderService.getOne(Wrappers.<ModelProvider>lambdaQuery()
                    .eq(ModelProvider::getProviderId, embeddingConfig.getProviderId())
                    .eq(ModelProvider::getEnabled, true));

            if (modelProvider == null) {
                throw new BizException(MessageConstants.AI_MODEL_MSG_1000);
            }

            List<Float> vectors = new ArrayList<>();
            for (float value : getEmbeddingModel(embeddingConfig.getProviderId(), embeddingConfig.getModel(),
                    embeddingConfig.getDimension())
                    .embed(text)) {
                vectors.add(value);
            }
            return vectors;
        } catch (Exception e) {
            log.error("获取嵌入向量失败", e);
            return new ArrayList<>();
        }
    }

    private synchronized EmbeddingModel getEmbeddingModel(String providerId, String model, Integer dimension) {
        // 获取模型提供商
        ModelProvider modelProvider = modelProviderService.getOne(Wrappers.<ModelProvider>lambdaQuery()
                .eq(ModelProvider::getProviderId, providerId)
                .eq(ModelProvider::getEnabled, true));

        if (modelProvider == null || StringUtils.isEmpty(model) || Objects.isNull(dimension)) {
            throw new BizException(MessageConstants.AI_SAMPLE_SQL_MSG_1000);
        }

        ModelProviderType modelProviderType = ModelProviderType.valueOf(modelProvider.getProviderType());
        return switch (modelProviderType) {
            case OPENAI -> createOpenAiEmbeddingModel(modelProvider, model, dimension);
            case OLLAMA -> createOllamaEmbeddingModel(modelProvider, model);
            default -> throw new UnsupportedOperationException("仅支持 OpenAI 和 Ollama 模型提供商");
        };
    }

    private EmbeddingModel createOpenAiEmbeddingModel(ModelProvider modelProvider, String embeddingModel,
            Integer dimension) {
        return new OpenAiEmbeddingModel(
                OpenAiEmbeddingOptions.builder()
                        .baseUrl(modelProvider.getBaseUrl())
                        .apiKey(modelProvider.getApiKey())
                        .model(embeddingModel)
                        .dimensions(dimension)
                        .build());
    }

    private EmbeddingModel createOllamaEmbeddingModel(ModelProvider modelProvider, String embeddingModel) {
        OllamaApi ollamaApi = OllamaApi.builder()
                .baseUrl(modelProvider.getBaseUrl())
                .build();

        return OllamaEmbeddingModel.builder()
                .ollamaApi(ollamaApi)
                .defaultOptions(OllamaEmbeddingOptions.builder()
                        .model(embeddingModel)
                        .build())
                .build();
    }
}
