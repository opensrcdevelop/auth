package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.ChatMessageHistoryResponseDto;
import cn.opensrcdevelop.ai.entity.ChatMessageHistory;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import com.baomidou.mybatisplus.extension.service.IService;
import java.time.LocalDateTime;
import java.util.List;

public interface ChatMessageHistoryService extends IService<ChatMessageHistory> {

    void createChatMessageHistory(String content, ChatContentType chatContentType);

    void createChatMessageHistory(ChatContentType chatContentType, String rewrittenQuestion, LocalDateTime time);

    void createChatMessageHistory(ChatContentType chatContentType, String chartId, String rewrittenQuestion,
            LocalDateTime time);

    void createUserChatMessageHistory(String content);

    List<ChatMessageHistoryResponseDto> listChatMessageHistory(String chatId);

    List<String> getUserHistoryQuestions(String chatId);

    void removeUserChatMessageHistory(String chatId);

    /**
     * 获取会话用户消息数量
     *
     * @param chatId
     *            对话ID
     * @return 用户消息数量
     */
    int countUserMessages(String chatId);
}
