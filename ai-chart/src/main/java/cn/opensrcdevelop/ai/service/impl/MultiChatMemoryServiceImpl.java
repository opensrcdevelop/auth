package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.MultiChatMemory;
import cn.opensrcdevelop.ai.mapper.MultiChatMemoryMapper;
import cn.opensrcdevelop.ai.service.MultiChatMemoryService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class MultiChatMemoryServiceImpl extends ServiceImpl<MultiChatMemoryMapper, MultiChatMemory> implements MultiChatMemoryService {
}
