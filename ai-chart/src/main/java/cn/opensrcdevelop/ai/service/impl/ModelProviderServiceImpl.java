package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.ModelProvider;
import cn.opensrcdevelop.ai.mapper.ModelProviderMapper;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class ModelProviderServiceImpl extends ServiceImpl<ModelProviderMapper, ModelProvider> implements ModelProviderService {
}
