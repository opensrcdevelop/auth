package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.dto.SampleSqlRequestDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.service.*;
import cn.opensrcdevelop.biz.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.util.TenantContextHolder;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import java.util.*;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class SampleSqlServiceImpl implements SampleSqlService {

    private static final String SIMILARITY_THRESHOLD_KEY = "chatbi.embedding.similarity.threshold";
    private static final double DEFAULT_THRESHOLD = 0.7;
    private static final int DEFAULT_LIMIT = 5;

    private final MilvusService milvusService;
    private final EmbeddingService embeddingService;
    private final ChatAnswerService chatAnswerService;
    private final SystemSettingService systemSettingService;

    @Override
    public List<SampleSqlDto> list(String dataSourceId) {
        String tenantCode = TenantContextHolder.getTenantCode();

        // 从 Milvus 获取该租户下所有向量
        List<SampleSqlDto> allResults = milvusService.listAll(tenantCode);

        // 如果指定了数据源，进行过滤
        if (dataSourceId != null && !dataSourceId.isEmpty()) {
            return allResults.stream()
                    .filter(dto -> dataSourceId.equals(dto.getDataSourceId()))
                    .collect(Collectors.toList());
        }

        return allResults;
    }

    @Override
    public void add(SampleSqlRequestDto request) {
        String tenantCode = TenantContextHolder.getTenantCode();
        String id = UUID.randomUUID().toString();

        SampleSqlDto dto = SampleSqlDto.builder()
                .id(id)
                .answerId("manual-" + id) // 手动添加的标记
                .question(request.getQuestion())
                .sql(request.getSql())
                .dataSourceId(request.getDataSourceId())
                .build();

        // 生成向量并插入
        List<Float> vector = embeddingService.embedText(request.getQuestion());
        if (vector.isEmpty()) {
            throw new RuntimeException("生成嵌入向量失败");
        }

        milvusService.insert(tenantCode, dto, vector);
        log.info("手动添加示例 SQL: id={}, dataSourceId={}", id, request.getDataSourceId());
    }

    @Override
    public void delete(String id) {
        String tenantCode = TenantContextHolder.getTenantCode();
        milvusService.deleteById(tenantCode, id);
        log.info("删除示例 SQL: id={}", id);
    }

    @Override
    public int syncFromLikes() {
        String tenantCode = TenantContextHolder.getTenantCode();

        // 获取当前租户下所有 LIKE 反馈的回答
        List<ChatAnswer> likes = chatAnswerService.list(new LambdaQueryWrapper<ChatAnswer>()
                .select(ChatAnswer::getAnswerId, ChatAnswer::getQuestion, ChatAnswer::getSql,
                        ChatAnswer::getDataSourceId)
                .eq(ChatAnswer::getFeedback, "LIKE")
                .isNotNull(ChatAnswer::getSql)
                .ne(ChatAnswer::getSql, ""));

        int syncedCount = 0;
        for (ChatAnswer answer : likes) {
            try {
                addToVectorStore(answer.getAnswerId());
                syncedCount++;
            } catch (Exception e) {
                log.error("同步示例 SQL 失败: answerId={}", answer.getAnswerId(), e);
            }
        }

        log.info("从 LIKE 反馈同步完成，共 {} 条", syncedCount);
        return syncedCount;
    }

    @Override
    public int rebuild() {
        String tenantCode = TenantContextHolder.getTenantCode();

        // 重建前先清空当前租户的 Collection
        milvusService.deleteAll(tenantCode);

        return syncFromLikes();
    }

    @Override
    public void addToVectorStore(String answerId) {
        String tenantCode = TenantContextHolder.getTenantCode();

        // 查询回答信息
        ChatAnswer answer = chatAnswerService.getById(answerId);
        if (answer == null || answer.getSql() == null || answer.getSql().isEmpty()) {
            log.warn("回答不存在或 SQL 为空: answerId={}", answerId);
            return;
        }

        SampleSqlDto dto = SampleSqlDto.builder()
                .id(UUID.randomUUID().toString())
                .answerId(answerId)
                .question(answer.getQuestion())
                .sql(answer.getSql())
                .dataSourceId(answer.getDataSourceId())
                .build();

        List<Float> vector = embeddingService.embedText(answer.getQuestion());
        if (vector.isEmpty()) {
            throw new RuntimeException("生成嵌入向量失败: answerId=" + answerId);
        }

        milvusService.insert(tenantCode, dto, vector);
        log.info("添加示例 SQL 到向量库: answerId={}", answerId);
    }

    @Override
    public void removeFromVectorStore(String answerId) {
        String tenantCode = TenantContextHolder.getTenantCode();
        milvusService.deleteByAnswerId(tenantCode, answerId);
        log.info("从向量库删除示例 SQL: answerId={}", answerId);
    }

    @Override
    public List<SampleSqlDto> search(String dataSourceId, String question) {
        String tenantCode = TenantContextHolder.getTenantCode();
        double threshold = getSimilarityThreshold();

        // 生成查询向量
        List<Float> vector = embeddingService.embedText(question);
        if (vector.isEmpty()) {
            log.warn("生成嵌入向量失败，返回空列表");
            return new ArrayList<>();
        }

        return milvusService.search(tenantCode, dataSourceId, vector, threshold, DEFAULT_LIMIT);
    }

    private double getSimilarityThreshold() {
        try {
            String value = systemSettingService.getValueByKey(SIMILARITY_THRESHOLD_KEY, "");
            if (value != null && !value.isEmpty()) {
                return Double.parseDouble(value);
            }
        } catch (Exception e) {
            log.warn("获取相似度阈值失败，使用默认值", e);
        }
        return DEFAULT_THRESHOLD;
    }
}
