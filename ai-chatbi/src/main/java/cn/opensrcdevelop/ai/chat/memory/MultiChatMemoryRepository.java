package cn.opensrcdevelop.ai.chat.memory;

import cn.opensrcdevelop.ai.entity.MultiChatMemory;
import cn.opensrcdevelop.ai.mapper.MultiChatMemoryMapper;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.metadata.MapperProxyMetadata;
import com.baomidou.mybatisplus.core.toolkit.MybatisUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.toolkit.SqlHelper;
import com.fasterxml.jackson.core.type.TypeReference;
import java.time.LocalDateTime;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.apache.ibatis.logging.Log;
import org.apache.ibatis.logging.LogFactory;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.ai.chat.memory.ChatMemoryRepository;
import org.springframework.ai.chat.messages.*;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class MultiChatMemoryRepository implements ChatMemoryRepository {
    private final Log log = LogFactory.getLog(this.getClass());

    private final MultiChatMemoryMapper multiChatMemoryMapper;

    @NonNull
    @Override
    public List<String> findConversationIds() {
        return CommonUtil
                .stream(multiChatMemoryMapper.selectList(Wrappers.<MultiChatMemory>lambdaQuery()
                        .select(MultiChatMemory::getChatId)
                        .eq(MultiChatMemory::getPromptTemplate,
                                ChatMemoryContextHolder.getChatMemoryContext().getPromptTemplate())))
                .map(MultiChatMemory::getChatId)
                .toList();
    }

    @NonNull
    @Override
    public List<Message> findByConversationId(@NonNull String conversationId) {
        return CommonUtil.stream(multiChatMemoryMapper.selectList(Wrappers.<MultiChatMemory>lambdaQuery()
                .select(MultiChatMemory::getContent, MultiChatMemory::getType)
                .eq(MultiChatMemory::getChatId, conversationId)
                .eq(MultiChatMemory::getPromptTemplate,
                        ChatMemoryContextHolder.getChatMemoryContext().getPromptTemplate())
                .orderByDesc(MultiChatMemory::getCreateTime)))
                .map(this::buildMessage)
                .toList();
    }

    public List<Message> findByConversationId(@NonNull String conversationId, int limit) {
        return CommonUtil.stream(multiChatMemoryMapper.selectList(Wrappers.<MultiChatMemory>lambdaQuery()
                .select(MultiChatMemory::getContent, MultiChatMemory::getType)
                .eq(MultiChatMemory::getChatId, conversationId)
                .eq(MultiChatMemory::getPromptTemplate,
                        ChatMemoryContextHolder.getChatMemoryContext().getPromptTemplate())
                .orderByDesc(MultiChatMemory::getCreateTime)
                .last("limit " + limit)))
                .map(this::buildMessage)
                .toList();
    }

    @Override
    public void saveAll(@NonNull String conversationId, @NonNull List<Message> messages) {
        List<MultiChatMemory> saveList = CommonUtil.stream(messages).map(message -> {
            MultiChatMemory multiChatMemory = new MultiChatMemory();
            multiChatMemory.setChatId(conversationId);
            multiChatMemory.setPromptTemplate(ChatMemoryContextHolder.getChatMemoryContext().getPromptTemplate());

            if (message instanceof ToolResponseMessage toolResponseMessage) {
                multiChatMemory.setContent(CommonUtil.serializeObject(toolResponseMessage.getResponses()));
            } else {
                multiChatMemory.setContent(message.getText());
            }
            multiChatMemory.setType(message.getMessageType().name());
            multiChatMemory.setCreateTime(LocalDateTime.now());

            return multiChatMemory;
        }).toList();

        MapperProxyMetadata mapperProxyMetadata = MybatisUtils.getMapperProxy(multiChatMemoryMapper);
        SqlSessionFactory sqlSessionFactory = MybatisUtils.getSqlSessionFactory(mapperProxyMetadata.getSqlSession());
        SqlHelper.executeBatch(sqlSessionFactory, log, saveList, saveList.size(),
                (sqlSession, entity) -> multiChatMemoryMapper.insert(entity));
    }

    @Override
    public void deleteByConversationId(@NonNull String conversationId) {
        multiChatMemoryMapper.delete(Wrappers.<MultiChatMemory>lambdaQuery()
                .eq(MultiChatMemory::getChatId, conversationId)
                .eq(MultiChatMemory::getPromptTemplate,
                        ChatMemoryContextHolder.getChatMemoryContext().getPromptTemplate()));
    }

    private Message buildMessage(MultiChatMemory multiChatMemory) {
        MessageType type = MessageType.valueOf(multiChatMemory.getType());
        String content = multiChatMemory.getContent();

        return switch (type) {
            case USER -> new UserMessage(content);
            case ASSISTANT -> new AssistantMessage(content);
            case SYSTEM -> new SystemMessage(content);
            case TOOL -> ToolResponseMessage.builder().responses(
                    CommonUtil.deserializeObject(content, new TypeReference<List<ToolResponseMessage.ToolResponse>>() {
                    })).build();
        };
    }
}
