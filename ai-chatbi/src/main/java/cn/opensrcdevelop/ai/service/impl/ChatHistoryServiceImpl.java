package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.ChatHistoryRequestDto;
import cn.opensrcdevelop.ai.dto.ChatHistoryResponseDto;
import cn.opensrcdevelop.ai.entity.ChatHistory;
import cn.opensrcdevelop.ai.mapper.ChatHistoryMapper;
import cn.opensrcdevelop.ai.service.ChatHistoryService;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import cn.opensrcdevelop.ai.service.MultiChatMemoryService;
import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.time.LocalDateTime;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class ChatHistoryServiceImpl extends ServiceImpl<ChatHistoryMapper, ChatHistory> implements ChatHistoryService {

    private final ChatMessageHistoryService chatMessageHistoryService;
    private final MultiChatMemoryService multiChatMemoryService;

    /**
     * 获取用户历史对话记录列表
     *
     * @param keyword
     *            对话标题检索关键词
     * @return 用户历史对话记录列表
     */
    @Override
    public List<ChatHistoryResponseDto> listUserChatHistory(String keyword) {
        // 1. 数据库操作
        List<ChatHistory> chatHistoryList;
        if (StringUtils.isNotEmpty(keyword)) {
            chatHistoryList = super.list(Wrappers.<ChatHistory>lambdaQuery()
                    .eq(ChatHistory::getUserId, SecurityContextHolder.getContext().getAuthentication().getName())
                    .like(ChatHistory::getTitle, keyword)
                    .orderByDesc(ChatHistory::getStartTime));
        } else {
            chatHistoryList = super.list(Wrappers.<ChatHistory>lambdaQuery()
                    .eq(ChatHistory::getUserId, SecurityContextHolder.getContext().getAuthentication().getName())
                    .orderByDesc(ChatHistory::getStartTime));
        }

        // 2. 属性设置
        return CommonUtil.stream(chatHistoryList).map(chatHistory -> ChatHistoryResponseDto.builder()
                .id(chatHistory.getChatId())
                .title(chatHistory.getTitle())
                .dataSourceId(chatHistory.getDataSourceId())
                .desc(chatHistory.getDescription())
                .start(chatHistory.getStartTime())
                .end(chatHistory.getEndTime()).build()).toList();
    }

    /**
     * 创建用户对话历史记录
     *
     * @param chatId
     *            对话ID
     * @param title
     *            标题
     * @param dataSourceId
     *            数据源ID
     */
    @Override
    public void createChatHistory(String chatId, String title, String dataSourceId) {
        ChatHistory chatHistory = new ChatHistory();
        chatHistory.setChatId(chatId);
        chatHistory.setUserId(SecurityContextHolder.getContext().getAuthentication().getName());
        chatHistory.setDataSourceId(dataSourceId);
        chatHistory.setTitle(title);
        chatHistory.setStartTime(LocalDateTime.now());
        super.save(chatHistory);
    }

    /**
     * 更新用户对话历史记录结束时间
     *
     * @param chatId
     *            对话ID
     * @param title
     *            标题
     */
    @Override
    public void updateChatHistory(String chatId, String title) {
        super.update(Wrappers.<ChatHistory>lambdaUpdate()
                .set(ChatHistory::getEndTime, LocalDateTime.now())
                .set(ChatHistory::getTitle, title)
                .eq(ChatHistory::getChatId, chatId));
    }

    /**
     * 删除用户对话历史记录
     *
     * @param chatId
     *            对话ID
     */
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.CHAT_BI, userOperation = UserOperationType.CHAT_BI_DELETE_HISTORY, success = "删除了对话（{{ #chatId }}）", fail = "删除对话（{{ #chatId }}）失败")
    @Transactional
    @Override
    public void removeUserChatHistory(String chatId) {
        // 1. 判断对话是否为当前用户持有
        boolean result = super.exists(Wrappers.<ChatHistory>lambdaQuery()
                .eq(ChatHistory::getChatId, chatId)
                .eq(ChatHistory::getUserId, SecurityContextHolder.getContext().getAuthentication().getName()));

        if (!result) {
            return;
        }

        // 2. 删除用户对话历史记录
        super.remove(Wrappers.<ChatHistory>lambdaQuery()
                .eq(ChatHistory::getChatId, chatId));

        // 3. 删除用户对话历史记录中的所有消息
        chatMessageHistoryService.removeUserChatMessageHistory(chatId);

        // 4. 删除对话记忆
        multiChatMemoryService.removeChatMemory(chatId);
    }

    /**
     * 更新用户对话历史记录
     *
     * @param requestDto
     *            对话历史请求
     */
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.CHAT_BI, userOperation = UserOperationType.CHAT_BI_UPDATE_HISTORY, success = "将对话（{{ #requestDto.id }}）标题更新为 {{ #requestDto.title }}", fail = "更新对话（{{ #requestDto.id }}）标题失败")
    @Override
    public void updateUserChatHistory(ChatHistoryRequestDto requestDto) {
        super.update(Wrappers.<ChatHistory>lambdaUpdate()
                .eq(ChatHistory::getChatId, requestDto.getId())
                .eq(ChatHistory::getUserId, SecurityContextHolder.getContext().getAuthentication().getName())
                .set(ChatHistory::getTitle, requestDto.getTitle()));
    }
}
