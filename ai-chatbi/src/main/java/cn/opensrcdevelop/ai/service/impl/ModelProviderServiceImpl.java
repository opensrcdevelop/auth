package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.ModelProviderResponseDto;
import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.ai.mapper.ModelProviderMapper;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@Service
public class ModelProviderServiceImpl extends ServiceImpl<ModelProviderMapper, ModelProvider> implements ModelProviderService {

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
                    .select(ModelProvider::getProviderId, ModelProvider::getProviderName, ModelProvider::getProviderType,
                            ModelProvider::getOptionalModels, ModelProvider::getDefaultModel)
                    .like(ModelProvider::getProviderName, keyword)
                    .orderByAsc(ModelProvider::getProviderName)
            );
        } else {
            modelProviderList = super.list(pageRequest, Wrappers.<ModelProvider>lambdaQuery()
                    .select(ModelProvider::getProviderId, ModelProvider::getProviderName, ModelProvider::getProviderType,
                            ModelProvider::getOptionalModels, ModelProvider::getDefaultModel)
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
                .type(modelProvider.getProviderType())
                .optionModels(StringUtils.isEmpty(modelProvider.getOptionalModels()) ? Collections.emptyList() : Arrays.asList(modelProvider.getOptionalModels().split(CommonConstants.COMMA)))
                .defaultModel(modelProvider.getDefaultModel())
                .build()
        ).toList();
        pageData.setList(data);
        return pageData;
    }
}
