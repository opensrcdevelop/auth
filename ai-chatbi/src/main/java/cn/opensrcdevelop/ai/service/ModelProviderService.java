package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.ModelProviderRequestDto;
import cn.opensrcdevelop.ai.dto.ModelProviderResponseDto;
import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

public interface ModelProviderService extends IService<ModelProvider> {

    List<ModelProviderResponseDto> enabledList();

    PageData<ModelProviderResponseDto> list(String keyword, int page, int size);

    ModelProviderResponseDto detail(String providerId);

    void createModelProvider(ModelProviderRequestDto requestDto);

    void updateModelProvider(ModelProviderRequestDto requestDto);

    void removeModelProvider(String providerId);
}
