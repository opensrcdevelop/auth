package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.MultiChatMemory;
import cn.opensrcdevelop.ai.mapper.MultiChatMemoryMapper;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.MultiChatMemoryService;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.ai.chat.messages.MessageType;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class MultiChatMemoryServiceImpl extends ServiceImpl<MultiChatMemoryMapper, MultiChatMemory> implements MultiChatMemoryService {

    /**
     * 获取用户历史提问
     *
     * @param chatId 对话 ID
     * @return 用户历史提问
     */
    @Override
    public List<String> getUserHistoryQuestions(String chatId) {
        return CommonUtil.stream(super.list(Wrappers.<MultiChatMemory>lambdaQuery()
                .select(MultiChatMemory::getContent)
                .eq(MultiChatMemory::getChatId, chatId)
                .eq(MultiChatMemory::getType, MessageType.USER.name())
                .eq(MultiChatMemory::getPromptTemplate, PromptTemplate.SELECT_TABLE)
                .orderByAsc(MultiChatMemory::getCreateTime)
        )).map(MultiChatMemory::getContent).toList();
    }
}
