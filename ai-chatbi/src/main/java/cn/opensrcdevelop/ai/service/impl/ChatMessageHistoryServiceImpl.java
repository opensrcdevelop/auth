package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.dto.ChatMessageHistoryResponseDto;
import cn.opensrcdevelop.ai.entity.ChatMessageHistory;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.ai.enums.ChatRole;
import cn.opensrcdevelop.ai.mapper.ChatMessageHistoryMapper;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Service
public class ChatMessageHistoryServiceImpl extends ServiceImpl<ChatMessageHistoryMapper, ChatMessageHistory> implements ChatMessageHistoryService {

    /**
     * 创建对话消息历史记录
     *
     * @param content         消息内容
     * @param chatContentType 消息类型
     */
    @Override
    public void createChatMessageHistory(String content, ChatContentType chatContentType) {
        ChatMessageHistory chatMessageHistory = new ChatMessageHistory();

        chatMessageHistory.setMessageId(CommonUtil.getUUIDV7String());
        chatMessageHistory.setCreateTime(LocalDateTime.now());
        chatMessageHistory.setChatId(ChatContext.getChatId());
        chatMessageHistory.setQuestionId(ChatContext.getQuestionId());
        chatMessageHistory.setUserId(SecurityContextHolder.getContext().getAuthentication().getName());
        chatMessageHistory.setContent(content);
        chatMessageHistory.setType(chatContentType.name());
        chatMessageHistory.setRole(ChatRole.ASSISTANT.name());

        asyncSaveChatMessageHistory(chatMessageHistory);
    }

    /**
     * 创建对话消息历史记录
     *
     * @param chatContentType   消息类型
     * @param rewrittenQuestion 重写后的问题
     * @param time              时间
     */
    @Override
    public void createChatMessageHistory(ChatContentType chatContentType, String rewrittenQuestion, LocalDateTime time) {
        ChatMessageHistory chatMessageHistory = new ChatMessageHistory();

        chatMessageHistory.setMessageId(CommonUtil.getUUIDV7String());
        chatMessageHistory.setCreateTime(LocalDateTime.now());
        chatMessageHistory.setChatId(ChatContext.getChatId());
        chatMessageHistory.setQuestionId(ChatContext.getQuestionId());
        chatMessageHistory.setUserId(SecurityContextHolder.getContext().getAuthentication().getName());
        chatMessageHistory.setType(chatContentType.name());
        chatMessageHistory.setRole(ChatRole.ASSISTANT.name());
        chatMessageHistory.setRewrittenQuestion(rewrittenQuestion);
        chatMessageHistory.setTime(time);

        asyncSaveChatMessageHistory(chatMessageHistory);
    }

    /**
     * 创建对话消息历史记录
     *
     * @param chatContentType   消息类型
     * @param answerId          回答ID
     * @param rewrittenQuestion 重写后的问题
     * @param time              时间
     */
    @Override
    public void createChatMessageHistory(ChatContentType chatContentType, String answerId, String rewrittenQuestion, LocalDateTime time) {
        ChatMessageHistory chatMessageHistory = new ChatMessageHistory();

        chatMessageHistory.setMessageId(CommonUtil.getUUIDV7String());
        chatMessageHistory.setCreateTime(LocalDateTime.now());
        chatMessageHistory.setChatId(ChatContext.getChatId());
        chatMessageHistory.setQuestionId(ChatContext.getQuestionId());
        chatMessageHistory.setUserId(SecurityContextHolder.getContext().getAuthentication().getName());
        chatMessageHistory.setType(chatContentType.name());
        chatMessageHistory.setRole(ChatRole.ASSISTANT.name());
        chatMessageHistory.setAnswerId(answerId);
        chatMessageHistory.setRewrittenQuestion(rewrittenQuestion);
        chatMessageHistory.setTime(time);

        asyncSaveChatMessageHistory(chatMessageHistory);
    }

    /**
     * 创建用户对话消息历史记录
     *
     * @param content 消息内容
     */
    @Override
    public void createUserChatMessageHistory(String content) {
        ChatMessageHistory chatMessageHistory = new ChatMessageHistory();

        chatMessageHistory.setMessageId(CommonUtil.getUUIDV7String());
        chatMessageHistory.setCreateTime(LocalDateTime.now());
        chatMessageHistory.setChatId(ChatContext.getChatId());
        chatMessageHistory.setQuestionId(ChatContext.getQuestionId());
        chatMessageHistory.setUserId(SecurityContextHolder.getContext().getAuthentication().getName());
        chatMessageHistory.setContent(content);
        chatMessageHistory.setType(ChatContentType.TEXT.name());
        chatMessageHistory.setRole(ChatRole.USER.name());

        asyncSaveChatMessageHistory(chatMessageHistory);
    }

    /**
     * 获取对话消息历史记录
     *
     * @param chatId 对话ID
     * @return 对话消息历史记录列表
     */
    @Override
    public List<ChatMessageHistoryResponseDto> listChatMessageHistory(String chatId) {
        // 1. 数据库操作
        List<ChatMessageHistory> chatMessageHistoryList = super.list(Wrappers.<ChatMessageHistory>lambdaQuery()
                .eq(ChatMessageHistory::getChatId, chatId)
                .eq(ChatMessageHistory::getUserId, SecurityContextHolder.getContext().getAuthentication().getName())
                .orderByAsc(ChatMessageHistory::getCreateTime));

        // 2. 属性编辑
        return CommonUtil.stream(chatMessageHistoryList).map(chatMessageHistory -> {
            ChatMessageHistoryResponseDto.ChatMessageHistoryResponseDtoBuilder builder = ChatMessageHistoryResponseDto.builder()
                    .id(chatMessageHistory.getMessageId())
                    .chatId(chatMessageHistory.getChatId())
                    .questionId(chatMessageHistory.getQuestionId())
                    .role(chatMessageHistory.getRole())
                    .type(chatMessageHistory.getType())
                    .rewrittenQuestion(chatMessageHistory.getRewrittenQuestion())
                    .time(chatMessageHistory.getTime())
                    .answerId(chatMessageHistory.getAnswerId());

            if (List.of(ChatContentType.TABLE, ChatContentType.CHART).contains(ChatContentType.valueOf(chatMessageHistory.getType()))) {
                builder.content(CommonUtil.deserializeObject(chatMessageHistory.getContent(), new TypeReference<Map<String, Object>>() {
                }));
            } else {
                builder.content(chatMessageHistory.getContent());
            }

            return builder.build();
        }).toList();
    }

    /**
     * 获取用户历史问题
     *
     * @param chatId 对话ID
     * @return 用户历史问题列表
     */
    @Override
    public List<String> getUserHistoryQuestions(String chatId) {
        List<ChatMessageHistory> chatMessageHistoryList = super.list(Wrappers.<ChatMessageHistory>lambdaQuery()
                .select(ChatMessageHistory::getMessageId, ChatMessageHistory::getContent)
                .eq(ChatMessageHistory::getChatId, chatId)
                .eq(ChatMessageHistory::getRole, ChatRole.USER.name())
                .orderByAsc(ChatMessageHistory::getCreateTime));

        return CommonUtil.stream(chatMessageHistoryList).map(ChatMessageHistory::getContent)
                .filter(StringUtils::isNotBlank)
                .map(StringUtils::trim)
                .distinct()
                .toList();
    }

    /**
     * 删除用户对话消息历史记录
     *
     * @param chatId 对话ID
     */
    @Override
    public void removeUserChatMessageHistory(String chatId) {
        // 1. 删除用户对话历史记录中的所有消息
        super.remove(Wrappers.<ChatMessageHistory>lambdaQuery()
                .eq(ChatMessageHistory::getChatId, chatId)
                .eq(ChatMessageHistory::getUserId, SecurityContextHolder.getContext().getAuthentication().getName())
        );
    }

    private void asyncSaveChatMessageHistory(ChatMessageHistory chatMessageHistory) {
        CompletableFuture.runAsync(() -> super.save(chatMessageHistory));
    }
}
