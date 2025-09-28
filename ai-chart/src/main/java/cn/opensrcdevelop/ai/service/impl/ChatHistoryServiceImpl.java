package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.ChatHistoryResponseDto;
import cn.opensrcdevelop.ai.entity.ChatHistory;
import cn.opensrcdevelop.ai.mapper.ChatHistoryMapper;
import cn.opensrcdevelop.ai.service.ChatHistoryService;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

@Service
public class ChatHistoryServiceImpl extends ServiceImpl<ChatHistoryMapper, ChatHistory> implements ChatHistoryService {

    /**
     * 获取用户历史对话记录列表
     *
     * @return 用户历史对话记录列表
     */
    @Override
    public List<ChatHistoryResponseDto> listUserChatHistory() {
        // 1. 数据库操作
        List<ChatHistory> chatHistoryList = super.list(Wrappers.<ChatHistory>lambdaQuery()
                .eq(ChatHistory::getUserId, SecurityContextHolder.getContext().getAuthentication().getName())
                .orderByDesc(ChatHistory::getStartTime));

        // 2. 属性设置
        return CommonUtil.stream(chatHistoryList).map(chatHistory -> ChatHistoryResponseDto.builder()
                .id(chatHistory.getChatId())
                .title(chatHistory.getTitle())
                .desc(chatHistory.getDescription())
                .start(chatHistory.getStartTime())
                .end(chatHistory.getEndTime()).build()
        ).toList();
    }

    /**
     * 创建用户对话历史记录
     *
     * @param chatId 对话ID
     * @param title  标题
     */
    @Override
    public void createChatHistory(String chatId, String title) {
        ChatHistory chatHistory = new ChatHistory();
        chatHistory.setChatId(chatId);
        chatHistory.setUserId(SecurityContextHolder.getContext().getAuthentication().getName());
        chatHistory.setTitle(title);
        chatHistory.setStartTime(LocalDateTime.now());
        super.save(chatHistory);
    }

    /**
     * 更新用户对话历史记录结束时间
     *
     * @param chatId 对话ID
     */
    @Override
    public void updateChatHistoryEndTime(String chatId) {
        super.update(Wrappers.<ChatHistory>lambdaUpdate()
                .set(ChatHistory::getEndTime, LocalDateTime.now())
                .eq(ChatHistory::getChatId, chatId));
    }
}
