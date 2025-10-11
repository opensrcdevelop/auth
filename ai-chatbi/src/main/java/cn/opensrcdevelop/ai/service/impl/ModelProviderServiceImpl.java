package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.dto.ModelProviderRequestDto;
import cn.opensrcdevelop.ai.dto.ModelProviderResponseDto;
import cn.opensrcdevelop.ai.dto.ModelResponseDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.ai.enums.ModelProviderType;
import cn.opensrcdevelop.ai.mapper.ModelProviderMapper;
import cn.opensrcdevelop.ai.service.ChatAnswerService;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ModelProviderServiceImpl extends ServiceImpl<ModelProviderMapper, ModelProvider> implements ModelProviderService {

    private final ChatAnswerService chatAnswerService;

    /**
     * 获取已启用的模型提供商列表
     *
     * @return 已启用的模型提供商列表
     */
    @Override
    public List<ModelProviderResponseDto> enabledList() {
        // 1.查询数据库
        List<ModelProvider> modelProviderList = super.list(Wrappers.<ModelProvider>lambdaQuery()
                .select(ModelProvider::getProviderId, ModelProvider::getProviderName, ModelProvider::getOptionalModels, ModelProvider::getDefaultModel)
                .eq(ModelProvider::getEnabled, true)
                .orderByAsc(ModelProvider::getProviderName));

        // 2. 属性设置
        return CommonUtil.stream(modelProviderList)
                .map(modelProvider -> {
                    ModelProviderResponseDto.ModelProviderResponseDtoBuilder builder = ModelProviderResponseDto.builder()
                            .id(modelProvider.getProviderId())
                            .name(modelProvider.getProviderName())
                            .defaultModel(modelProvider.getDefaultModel());

                    if (StringUtils.isNotEmpty(modelProvider.getOptionalModels())) {
                        builder.optionalModels(
                                CommonUtil.stream(Arrays.asList(modelProvider.getOptionalModels().split(CommonConstants.COMMA)))
                                        .map(model -> ModelResponseDto.builder().name(model).build())
                                        .toList()
                        );
                    }
                    return builder.build();
                })
                .toList();
    }

    /**
     * 获取模型提供商列表
     *
     * @param keyword 模型提供商名称检索关键字
     * @param page    页数
     * @param size    条数
     * @return 模型提供商列表
     */
    @Override
    public PageData<ModelProviderResponseDto> list(String keyword, int page, int size) {
        // 1. 查询数据库
        List<ModelProvider> modelProviderList;
        Page<ModelProvider> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            modelProviderList = super.list(pageRequest, Wrappers.<ModelProvider>lambdaQuery()
                    .select(ModelProvider::getProviderId,
                            ModelProvider::getProviderName,
                            ModelProvider::getProviderType,
                            ModelProvider::getEnabled)
                    .like(ModelProvider::getProviderName, keyword)
                    .orderByAsc(ModelProvider::getProviderName)
            );
        } else {
            modelProviderList = super.list(pageRequest, Wrappers.<ModelProvider>lambdaQuery()
                    .select(ModelProvider::getProviderId,
                            ModelProvider::getProviderName,
                            ModelProvider::getProviderType,
                            ModelProvider::getEnabled)
                    .orderByAsc(ModelProvider::getProviderName)
            );
        }

        // 2. 属性编辑
        PageData<ModelProviderResponseDto> pageData = new PageData<>();
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());

        List<ModelProviderResponseDto> data = CommonUtil.stream(modelProviderList).map(modelProvider -> ModelProviderResponseDto.builder()
                .id(modelProvider.getProviderId())
                .name(modelProvider.getProviderName())
                .type(ModelProviderType.valueOf(modelProvider.getProviderType()).getDisplayName())
                .enabled(modelProvider.getEnabled())
                .build()
        ).toList();
        pageData.setList(data);
        return pageData;
    }

    /**
     * 获取模型提供商详情
     *
     * @param providerId 模型提供商ID
     * @return 模型提供商详情
     */
    @Override
    public ModelProviderResponseDto detail(String providerId) {
        // 1. 查询数据库
        ModelProvider modelProvider = super.getById(providerId);
        if (Objects.isNull(modelProvider)) {
            return ModelProviderResponseDto.builder().build();
        }

        // 2. 属性编辑
        ModelProviderResponseDto.ModelProviderResponseDtoBuilder builder = ModelProviderResponseDto.builder()
                .id(modelProvider.getProviderId())
                .name(modelProvider.getProviderName())
                .type(modelProvider.getProviderType())
                .apiKey(modelProvider.getApiKey())
                .baseUrl(modelProvider.getBaseUrl())
                .maxTokens(modelProvider.getMaxTokens())
                .temperature(modelProvider.getTemperature())
                .defaultModel(modelProvider.getDefaultModel());

        if (StringUtils.isNotEmpty(modelProvider.getOptionalModels())) {
            builder.optionalModels(
                    CommonUtil.stream(Arrays.asList(modelProvider.getOptionalModels().split(CommonConstants.COMMA)))
                            .map(model -> {
                                Map<String, Object> tokensMap = chatAnswerService.getMap(Wrappers.<ChatAnswer>query()
                                        .select("COALESCE(SUM(req_tokens), 0) as req_tokens",
                                                "COALESCE(SUM(rep_tokens), 0) as rep_tokens")
                                        .eq("model_provider_id", providerId)
                                        .eq("model", model));
                                return ModelResponseDto.builder()
                                        .name(model)
                                        .usedReqTokens((Long) tokensMap.get("req_tokens"))
                                        .usedRepTokens((Long) tokensMap.get("rep_tokens"))
                                        .build();

                            })
                            .toList()
            );
        }


        return builder.build();
    }

    /**
     * 创建模型提供商
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.CHAT_BI_MODEL_PROVIDER,
            sysOperation = SysOperationType.CREATE,
            success = "创建了模型提供商（{{ @linkGen.toLink(#modelProviderId, T(ResourceType).CHAT_BI_DATA_SOURCE) }}）",
            fail = "创建模型提供商（{{ #requestDto.name }}）失败"
    )
    @Transactional
    @Override
    public void createModelProvider(ModelProviderRequestDto requestDto) {
        // 1. 属性编辑
        String modelProviderId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("modelProviderId", modelProviderId);

        ModelProvider modelProvider = new ModelProvider();
        modelProvider.setProviderId(modelProviderId);
        modelProvider.setProviderName(requestDto.getName());
        modelProvider.setProviderType(requestDto.getType().name());
        modelProvider.setBaseUrl(requestDto.getBaseUrl());
        modelProvider.setApiKey(requestDto.getApiKey());
        modelProvider.setOptionalModels(CommonUtil.stream(requestDto.getOptionalModels())
                .distinct()
                .collect(Collectors.joining(CommonConstants.COMMA)));
        modelProvider.setDefaultModel(requestDto.getDefaultModel());
        modelProvider.setTemperature(requestDto.getTemperature());
        modelProvider.setMaxTokens(requestDto.getMaxTokens());
        modelProvider.setEnabled(true);

        // 2. 数据库操作
        super.save(modelProvider);
    }

    /**
     * 更新模型提供商
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.CHAT_BI_MODEL_PROVIDER,
            sysOperation = SysOperationType.UPDATE,
            success = "更新了模型提供商（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).CHAT_BI_MODEL_PROVIDER) }}）",
            fail = "更新模型提供商（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).CHAT_BI_MODEL_PROVIDER) }}）失败"
    )
    @Transactional
    @Override
    public void updateModelProvider(ModelProviderRequestDto requestDto) {

        String modelProviderId = requestDto.getId();

        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        var rawModelProvider = super.getById(modelProviderId);
        if (Objects.isNull(rawModelProvider)) {
            return;
        }

        compareObjBuilder.id(modelProviderId);
        compareObjBuilder.before(rawModelProvider);

        // 2. 属性编辑
        ModelProvider updateModelProvider = new ModelProvider();
        updateModelProvider.setProviderId(requestDto.getId());
        updateModelProvider.setProviderName(requestDto.getName());
        updateModelProvider.setBaseUrl(requestDto.getBaseUrl());
        updateModelProvider.setApiKey(requestDto.getApiKey());
        updateModelProvider.setTemperature(requestDto.getTemperature());
        updateModelProvider.setMaxTokens(requestDto.getMaxTokens());
        updateModelProvider.setDefaultModel(requestDto.getDefaultModel());
        updateModelProvider.setVersion(rawModelProvider.getVersion());

        if (CollectionUtils.isNotEmpty(requestDto.getOptionalModels())) {
            // 2.1 检查是否删除了默认模型
            if (!requestDto.getOptionalModels().contains(rawModelProvider.getDefaultModel())) {
                throw new BizException(MessageConstants.AI_MODEL_MSG_1001);
            }

            updateModelProvider.setOptionalModels(CommonUtil.stream(requestDto.getOptionalModels())
                    .distinct()
                    .collect(Collectors.joining(CommonConstants.COMMA)));
        }

        if (Objects.nonNull(requestDto.getEnabled())) {
            updateModelProvider.setEnabled(requestDto.getEnabled());
        }

        // 3. 数据库操作
        super.updateById(updateModelProvider);

        compareObjBuilder.after(super.getById(modelProviderId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 删除模型提供商
     *
     * @param providerId 模型提供商ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.CHAT_BI_MODEL_PROVIDER,
            sysOperation = SysOperationType.DELETE,
            success = "删除了模型提供商（{{ @linkGen.toLink(#providerId, T(ResourceType).CHAT_BI_MODEL_PROVIDER) }}）",
            fail = "删除模型提供商（{{ @linkGen.toLink(#providerId, T(ResourceType).CHAT_BI_MODEL_PROVIDER) }}）失败"
    )
    @Transactional
    @Override
    public void removeModelProvider(String providerId) {
        // 1. 数据库操作
        super.removeById(providerId);
    }
}
