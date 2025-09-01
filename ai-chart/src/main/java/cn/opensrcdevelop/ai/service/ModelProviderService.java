package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.ModelProviderResponseDto;
import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

public interface ModelProviderService extends IService<ModelProvider> {

    PageData<ModelProviderResponseDto> list(String keyword, int page, int size);
}
