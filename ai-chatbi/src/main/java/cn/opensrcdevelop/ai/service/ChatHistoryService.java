package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.ChatHistoryRequestDto;
import cn.opensrcdevelop.ai.dto.ChatHistoryResponseDto;
import cn.opensrcdevelop.ai.entity.ChatHistory;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

public interface ChatHistoryService extends IService<ChatHistory> {

    List<ChatHistoryResponseDto> listUserChatHistory(String keyword);

    void createChatHistory(String chatId, String title, String dataSourceId);

    void updateChatHistory(String chatId, String title);

    void removeUserChatHistory(String chatId);

    void updateUserChatHistory(ChatHistoryRequestDto requestDto);
}
