package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.MultiChatMemory;
import cn.opensrcdevelop.ai.mapper.MultiChatMemoryMapper;
import cn.opensrcdevelop.ai.service.MultiChatMemoryService;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class MultiChatMemoryServiceImpl extends ServiceImpl<MultiChatMemoryMapper, MultiChatMemory> implements MultiChatMemoryService {

    /**
     * 删除对话记忆
     *
     * @param chatId 对话ID
     */
    @Override
    public void removeChatMessageHistory(String chatId) {
        super.remove(Wrappers.<MultiChatMemory>lambdaQuery().eq(MultiChatMemory::getChatId, chatId));
    }
}
