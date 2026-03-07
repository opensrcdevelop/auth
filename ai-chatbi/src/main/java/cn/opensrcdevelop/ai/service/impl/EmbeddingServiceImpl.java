package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.ai.service.EmbeddingService;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import cn.opensrcdevelop.biz.biz.service.system.SystemSettingService;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import java.util.ArrayList;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.openai.OpenAiEmbeddingModel;
import org.springframework.ai.openai.OpenAiEmbeddingOptions;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Slf4j
@Service
@RequiredArgsConstructor
public class EmbeddingServiceImpl implements EmbeddingService {

    private static final String EMBEDDING_PROVIDER_ID_KEY = "chatbi.embedding.provider.id";
    private static final int DEFAULT_DIMENSION = 1536;

    private final ModelProviderService modelProviderService;
    private final SystemSettingService systemSettingService;

    @Override
    public List<Float> embedText(String text) {
        String providerId = systemSettingService.getValueByKey(EMBEDDING_PROVIDER_ID_KEY, "");

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

            // 创建嵌入模型
            OpenAiApi openAiApi = OpenAiApi.builder()
                    .baseUrl(modelProvider.getBaseUrl())
                    .apiKey(modelProvider.getApiKey())
                    .build();

            OpenAiEmbeddingModel embeddingModel = OpenAiEmbeddingModel.builder()
                    .openAiApi(openAiApi)
                    .defaultOptions(OpenAiEmbeddingOptions.builder()
                            .model(modelProvider.getDefaultModel())
                            .build())
                    .build();

            // 调用嵌入 API
            List<String> texts = List.of(text);
            var response = embeddingModel.embed(texts);

            if (CollectionUtils.isEmpty(response.getResults())) {
                log.warn("嵌入向量为空");
                return new ArrayList<>();
            }

            // 转换为 Float 列表
            List<Float> vectors = new ArrayList<>();
            for (Double value : response.getResults().get(0).getEmbedding()) {
                vectors.add(value.floatValue());
            }

            log.debug("Text embedded successfully, dimension: {}", vectors.size());
            return vectors;
        } catch (Exception e) {
            log.error("获取嵌入向量失败", e);
            return new ArrayList<>();
        }
    }

    @Override
    public int getDimension() {
        // 实际应该从嵌入模型配置中获取维度
        // 这里简化处理，返回默认值
        return DEFAULT_DIMENSION;
    }
}
