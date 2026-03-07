package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.dto.SampleSqlRequestDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.service.*;
import cn.opensrcdevelop.auth.biz.entity.system.SystemSetting;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
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

    private final SampleSqlVectorStoreService sampleSqlVectorStoreService;
    private final EmbeddingService embeddingService;
    private final ChatAnswerService chatAnswerService;
    private final SystemSettingService systemSettingService;

    @Override
    public List<SampleSqlDto> list(String dataSourceId) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();

        // 从 Milvus 获取该租户下所有向量（分页获取）
        List<SampleSqlDto> allResults = new ArrayList<>();
        int offset = 0;
        int pageSize = 100;
        List<SampleSqlDto> pageResults;
        do {
            pageResults = sampleSqlVectorStoreService.list(tenantCode, offset, pageSize);
            allResults.addAll(pageResults);
            offset += pageSize;
        } while (!pageResults.isEmpty());

        // 如果指定了数据源，进行过滤
        if (dataSourceId != null && !dataSourceId.isEmpty()) {
            return allResults.stream()
                    .filter(dto -> dataSourceId.equals(dto.getDataSourceId()))
                    .collect(Collectors.toList());
        }

        return allResults;
    }

    @Override
    public PageData<SampleSqlDto> list(String dataSourceId, long current, long size) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();

        // 使用 Milvus 原生分页查询
        PageData<SampleSqlDto> pageData = sampleSqlVectorStoreService.list(tenantCode, current, size);

        // 如果指定了数据源，进行过滤
        if (dataSourceId != null && !dataSourceId.isEmpty()) {
            List<SampleSqlDto> filteredList = pageData.getList().stream()
                    .filter(dto -> dataSourceId.equals(dto.getDataSourceId()))
                    .collect(Collectors.toList());

            // 重新计算分页信息
            long total = filteredList.size();
            long pages = (size > 0) ? (total + size - 1) / size : 0;

            pageData.setTotal(total);
            pageData.setPages(pages);
            pageData.setList(filteredList);
        }

        return pageData;
    }

    @Override
    public void add(SampleSqlRequestDto request) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
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

        sampleSqlVectorStoreService.insert(tenantCode, dto, vector);
        log.info("手动添加示例 SQL: id={}, dataSourceId={}", id, request.getDataSourceId());
    }

    @Override
    public void delete(String id) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        sampleSqlVectorStoreService.deleteById(tenantCode, id);
        log.info("删除示例 SQL: id={}", id);
    }

    @Override
    public int syncFromLikes() {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        log.info("开始同步 LIKE 反馈，租户: {}", tenantCode);

        // 先查询所有有 feedback 的记录
        List<ChatAnswer> allWithFeedback = chatAnswerService.list(new LambdaQueryWrapper<ChatAnswer>()
                .select(ChatAnswer::getAnswerId, ChatAnswer::getQuestion, ChatAnswer::getSql,
                        ChatAnswer::getDataSourceId, ChatAnswer::getFeedback)
                .isNotNull(ChatAnswer::getFeedback)
                .ne(ChatAnswer::getFeedback, ""));

        log.info("查询到 {} 条有 feedback 的记录", allWithFeedback.size());
        for (ChatAnswer a : allWithFeedback) {
            log.info("answerId={}, feedback={}", a.getAnswerId(), a.getFeedback());
        }

        // 获取当前租户下所有 LIKE 反馈的回答
        List<ChatAnswer> likes = allWithFeedback.stream()
                .filter(a -> "LIKE".equalsIgnoreCase(a.getFeedback()))
                .filter(a -> a.getSql() != null && !a.getSql().isEmpty())
                .collect(Collectors.toList());

        log.info("过滤后有 {} 条 LIKE 反馈且 SQL 不为空", likes.size());
        for (ChatAnswer a : likes) {
            log.info("answerId={}, question={}, sql={}", a.getAnswerId(), a.getQuestion(), a.getSql());
        }

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
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();

        // 重建前先清空当前租户的 Collection
        sampleSqlVectorStoreService.deleteAll(tenantCode);

        return syncFromLikes();
    }

    @Override
    public void addToVectorStore(String answerId) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();

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

        sampleSqlVectorStoreService.insert(tenantCode, dto, vector);
        log.info("添加示例 SQL 到向量库: answerId={}", answerId);
    }

    @Override
    public void removeFromVectorStore(String answerId) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        sampleSqlVectorStoreService.deleteByAnswerId(tenantCode, answerId);
        log.info("从向量库删除示例 SQL: answerId={}", answerId);
    }

    @Override
    public PageData<SampleSqlDto> search(String dataSourceId, String question, long current, long size) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        double threshold = getSimilarityThreshold();

        // 生成查询向量
        List<Float> vector = embeddingService.embedText(question);
        if (vector.isEmpty()) {
            log.warn("生成嵌入向量失败，返回空列表");
            PageData<SampleSqlDto> emptyPageData = new PageData<>();
            emptyPageData.setCurrent(current);
            emptyPageData.setSize(size);
            emptyPageData.setTotal(0L);
            emptyPageData.setPages(0L);
            emptyPageData.setList(new ArrayList<>());
            return emptyPageData;
        }

        return sampleSqlVectorStoreService.search(tenantCode, dataSourceId, vector, threshold, current, size);
    }

    private double getSimilarityThreshold() {
        try {
            SystemSetting setting = systemSettingService.getByKey(SIMILARITY_THRESHOLD_KEY);
            if (setting != null && setting.getValue() != null && !setting.getValue().isEmpty()) {
                return Double.parseDouble(setting.getValue());
            }
        } catch (Exception e) {
            log.warn("获取相似度阈值失败，使用默认值", e);
        }
        return DEFAULT_THRESHOLD;
    }
}
