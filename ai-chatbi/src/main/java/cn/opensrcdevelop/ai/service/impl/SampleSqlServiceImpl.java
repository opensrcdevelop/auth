package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.constants.SystemSettingConstants;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.dto.SampleSqlEmbeddingConfigDto;
import cn.opensrcdevelop.ai.dto.SampleSqlRequestDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.enums.Feedback;
import cn.opensrcdevelop.ai.service.ChatAnswerService;
import cn.opensrcdevelop.ai.service.SampleSqlEmbeddingService;
import cn.opensrcdevelop.ai.service.SampleSqlService;
import cn.opensrcdevelop.ai.service.SampleSqlVectorStoreService;
import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.Strings;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

@Slf4j
@Service
@RequiredArgsConstructor
public class SampleSqlServiceImpl implements SampleSqlService {

    private static final double DEFAULT_THRESHOLD = 0.6;

    private final SampleSqlVectorStoreService sampleSqlVectorStoreService;
    private final SampleSqlEmbeddingService embeddingService;
    private final ChatAnswerService chatAnswerService;
    private final SystemSettingService systemSettingService;

    /**
     * 查询指定数据源下的所有示例 SQL
     *
     * @param dataSourceId
     *            数据源ID
     * @return 示例 SQL 列表
     */
    @Override
    public List<SampleSqlDto> list(String dataSourceId, String question, long offset, int limit) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();

        return sampleSqlVectorStoreService.list(tenantCode, dataSourceId, question, offset, limit);
    }

    /**
     * 添加一个示例 SQL
     *
     * @param requestDto
     *            示例 SQL 请求参数
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.CHAT_BI_SAMPLE_SQL, sysOperation = SysOperationType.CREATE, success = "添加了示例 SQL，ID: {{ #sampleSqlId }}", fail = "添加示例 SQL 失败, 问题：{{ #request.question }}，SQL: {{ #request.sql }}")
    @Override
    public void add(SampleSqlRequestDto requestDto) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();

        String sampleSqlId = CommonUtil.getUUIDV7String();

        SampleSqlDto dto = SampleSqlDto.builder()
                .id(sampleSqlId)
                .question(requestDto.getQuestion())
                .sql(requestDto.getSql())
                .dataSourceId(requestDto.getDataSourceId())
                .build();

        List<Float> vector = embeddingService.embedText(requestDto.getQuestion());
        if (vector.isEmpty()) {
            throw new BizException(MessageConstants.AI_SAMPLE_SQL_MSG_1000);
        }

        sampleSqlVectorStoreService.insert(tenantCode, dto, vector);

        AuditContext.setSpelVariable("sampleSqlId", sampleSqlId);
    }

    /**
     * 删除指定 ID 的示例 SQL
     *
     * @param id
     *            示例 SQL ID
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.CHAT_BI_SAMPLE_SQL, sysOperation = SysOperationType.DELETE, success = "删除了示例 SQL，ID: {{ #id }}", fail = "删除示例 SQL 失败，ID: {{ #id }}")
    @Override
    public void delete(String id) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        sampleSqlVectorStoreService.deleteById(tenantCode, id);
    }

    /**
     * 从 LIKE 反馈中同步示例 SQL 到向量存储
     *
     * @return 同步成功的记录数
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.CHAT_BI_SAMPLE_SQL, sysOperation = SysOperationType.CREATE, success = "从 LIKE 反馈同步了示例 SQL，共 {{ #syncedCount }} 条", fail = "从 LIKE 反馈同步示例 SQL 失败")
    @Override
    public int syncFromLikes() {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        log.info("开始同步 LIKE 反馈，租户: {}", tenantCode);

        List<ChatAnswer> likes = chatAnswerService.list(Wrappers.<ChatAnswer>lambdaQuery()
                .select(ChatAnswer::getAnswerId, ChatAnswer::getQuestion, ChatAnswer::getSql,
                        ChatAnswer::getDataSourceId, ChatAnswer::getFeedback)
                .eq(ChatAnswer::getFeedback, Feedback.LIKE.name()));

        sampleSqlVectorStoreService.createCollectionIfNotExists(tenantCode);
        AtomicInteger syncedCount = new AtomicInteger();
        CommonUtil.stream(likes)
                .filter(like -> StringUtils.isNotBlank(like.getQuestion()) && StringUtils.isNotBlank(like.getSql()))
                .forEach(like -> {
                    sampleSqlVectorStoreService.deleteByAnswerId(tenantCode, like.getAnswerId());

                    SampleSqlDto dto = SampleSqlDto.builder()
                            .id(CommonUtil.getUUIDV7String())
                            .answerId(like.getAnswerId())
                            .question(like.getQuestion())
                            .sql(like.getSql())
                            .dataSourceId(like.getDataSourceId())
                            .build();

                    List<Float> vector = embeddingService.embedText(like.getQuestion());
                    if (vector.isEmpty()) {
                        throw new BizException(MessageConstants.AI_SAMPLE_SQL_MSG_1000);
                    }
                    sampleSqlVectorStoreService.insert(tenantCode, dto, vector);
                    syncedCount.getAndIncrement();
                });
        log.info("从 LIKE 反馈同步完成，共 {} 条", syncedCount.get());

        AuditContext.setSpelVariable("syncedCount", syncedCount.get());
        return syncedCount.get();
    }

    /**
     * 添加指定回答 ID 的示例 SQL 到向量存储
     *
     * @param answerId
     *            回答 ID
     */
    @Override
    public void addToVectorStore(String answerId) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();

        // 查询回答信息
        ChatAnswer answer = chatAnswerService.getById(answerId);
        if (Objects.isNull(answer) || StringUtils.isEmpty(answer.getQuestion())
                || StringUtils.isEmpty(answer.getSql())) {
            log.warn("回答不存在或 SQL 为空: answerId={}", answerId);
            return;
        }

        SampleSqlDto dto = SampleSqlDto.builder()
                .id(CommonUtil.getUUIDV7String())
                .answerId(answerId)
                .question(answer.getQuestion())
                .sql(answer.getSql())
                .dataSourceId(answer.getDataSourceId())
                .build();

        List<Float> vector = embeddingService.embedText(answer.getQuestion());
        if (vector.isEmpty()) {
            throw new BizException(MessageConstants.AI_SAMPLE_SQL_MSG_1000);
        }

        sampleSqlVectorStoreService.insert(tenantCode, dto, vector);
    }

    /**
     * 删除指定回答 ID 的示例 SQL 从向量存储
     *
     * @param answerId
     *            回答 ID
     */
    @Override
    public void removeFromVectorStore(String answerId) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        sampleSqlVectorStoreService.deleteByAnswerId(tenantCode, answerId);
    }

    /**
     * 搜索指定数据源 ID 下与问题最相似的示例 SQL
     *
     * @param dataSourceId
     *            数据源 ID
     * @param question
     *            问题
     * @param topK
     *            返回的最大结果数
     * @return 最相似的示例 SQL 列表
     */
    @Override
    public List<SampleSqlDto> search(String dataSourceId, String question, Integer topK) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        double threshold = getSimilarityThreshold();

        // 如果 topK 为 null，则使用配置中的 topK 值
        Integer effectiveTopK = topK;
        if (effectiveTopK == null) {
            effectiveTopK = getTopK();
        }

        List<Float> vector = embeddingService.embedText(question);
        if (vector.isEmpty()) {
            throw new BizException(MessageConstants.AI_SAMPLE_SQL_MSG_1000);
        }

        return sampleSqlVectorStoreService.search(tenantCode, dataSourceId, vector, threshold, effectiveTopK);
    }

    /**
     * 获取示例 SQL 嵌入模型配置
     *
     * @return 示例 SQL 嵌入模型配置
     */
    @Override
    public SampleSqlEmbeddingConfigDto getEmbeddingConfig() {
        return systemSettingService.getSystemSetting(SystemSettingConstants.SAMPLE_SQL_EMBEDDING_CONFIG,
                SampleSqlEmbeddingConfigDto.class);
    }

    /**
     * 更新示例 SQL 嵌入模型配置
     *
     * @param configDto
     *            示例 SQL 嵌入模型配置
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.CHAT_BI_SAMPLE_SQL, sysOperation = SysOperationType.UPDATE, success = "更新了示例 SQL 嵌入模型配置", fail = "更新示例 SQL 嵌入模型配置失败")
    @Override
    public void updateEmbeddingConfig(SampleSqlEmbeddingConfigDto configDto) {
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        SampleSqlEmbeddingConfigDto rawEmbeddingConfigDto = systemSettingService.getSystemSetting(
                SystemSettingConstants.SAMPLE_SQL_EMBEDDING_CONFIG, SampleSqlEmbeddingConfigDto.class);

        compareObjBuilder.before(rawEmbeddingConfigDto);
        compareObjBuilder.after(configDto);

        configDto.setPreviousModel(rawEmbeddingConfigDto.getModel());
        configDto.setPreviousDimension(rawEmbeddingConfigDto.getDimension());

        systemSettingService.saveSystemSetting(SystemSettingConstants.SAMPLE_SQL_EMBEDDING_CONFIG, configDto);

        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 判断是否需要重新构建示例 SQL 索引
     *
     * @param configDto
     *            示例 SQL 嵌入模型配置
     * @return 是否需要重新构建索引
     */
    @Override
    public boolean needRebuildIndex(SampleSqlEmbeddingConfigDto configDto) {
        SampleSqlEmbeddingConfigDto rawEmbeddingConfigDto = systemSettingService.getSystemSetting(
                SystemSettingConstants.SAMPLE_SQL_EMBEDDING_CONFIG, SampleSqlEmbeddingConfigDto.class);
        return !Strings.CS.equals(rawEmbeddingConfigDto.getModel(), configDto.getModel())
                || !Objects.equals(rawEmbeddingConfigDto.getDimension(), configDto.getDimension());
    }

    /**
     * 重新构建示例 SQL 索引
     *
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.CHAT_BI_SAMPLE_SQL, sysOperation = SysOperationType.UPDATE, success = "重新构建了示例 SQL 索引", fail = "重新构建示例 SQL 索引失败")
    @Override
    public void rebuildIndex() {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();

        // 1. 获取之前存储的示例 SQL 向量
        long total = sampleSqlVectorStoreService.count(tenantCode, null, null);
        List<SampleSqlDto> rawSqlDtoList = sampleSqlVectorStoreService.list(tenantCode, null, null, 0, (int) total);

        // 2. 删除旧索引
        sampleSqlVectorStoreService.deleteAll(tenantCode);

        // 3. 重新构建索引
        rawSqlDtoList.forEach(dto -> {
            List<Float> vector = embeddingService.embedText(dto.getQuestion());
            if (vector.isEmpty()) {
                throw new BizException(MessageConstants.AI_SAMPLE_SQL_MSG_1000);
            }
            sampleSqlVectorStoreService.insert(tenantCode, dto, vector);
        });
    }

    /**
     * 统计指定数据源 ID 下与问题最相似的示例 SQL 数量
     *
     * @param dataSourceId
     *            数据源 ID
     * @param question
     *            问题
     * @return 最相似的示例 SQL 数量
     */
    @Override
    public long count(String dataSourceId, String question) {
        String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
        return sampleSqlVectorStoreService.count(tenantCode, dataSourceId, question);
    }

    private double getSimilarityThreshold() {
        try {
            SampleSqlEmbeddingConfigDto embeddingConfigDto = systemSettingService.getSystemSetting(
                    SystemSettingConstants.SAMPLE_SQL_EMBEDDING_CONFIG, SampleSqlEmbeddingConfigDto.class);
            if (Objects.nonNull(embeddingConfigDto) && Objects.nonNull(embeddingConfigDto.getSimilarityThreshold())) {
                return embeddingConfigDto.getSimilarityThreshold();
            }
        } catch (Exception e) {
            log.warn("获取相似度阈值失败，使用默认值", e);
        }
        return DEFAULT_THRESHOLD;
    }

    private Integer getTopK() {
        try {
            SampleSqlEmbeddingConfigDto embeddingConfigDto = systemSettingService.getSystemSetting(
                    SystemSettingConstants.SAMPLE_SQL_EMBEDDING_CONFIG, SampleSqlEmbeddingConfigDto.class);
            if (Objects.nonNull(embeddingConfigDto) && Objects.nonNull(embeddingConfigDto.getTopK())) {
                return embeddingConfigDto.getTopK();
            }
        } catch (Exception e) {
            log.warn("获取 topK 失败，使用默认值", e);
        }
        return 10;
    }
}
