package cn.opensrcdevelop.ai.component;

import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.ai.service.csv.CsvParseService;
import cn.opensrcdevelop.auth.biz.constants.AsyncTaskTypeEnum;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutor;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutorAnno;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

/**
 * CSV 异步解析任务执行器
 * <p>
 * 负责从 S3 下载 CSV 文件并解析表结构，保存到数据库
 * </p>
 */
@Slf4j
@Component
@RequiredArgsConstructor
@AsyncTaskExecutorAnno(taskType = "CSV_PARSE")
public class CsvParseAsyncTaskExecutor implements AsyncTaskExecutor {

    public static final String TASK_NAME = "CSV 文件解析";

    private final CsvParseService csvParseService;
    private final TableService tableService;
    private final TableFieldService tableFieldService;

    @Override
    public String getTaskType() {
        return AsyncTaskTypeEnum.CSV_PARSE.getCode();
    }

    @Override
    @SuppressWarnings("java:S3776")
    public void execute(String taskId, String taskParams, TaskExecutionContext context) {
        try {
            context.updateProgress(10);

            // 解析任务参数
            Map<String, Object> params = CommonUtil.deserializeObject(taskParams,
                    new TypeReference<Map<String, Object>>() {
                    });

            String dataSourceId = (String) params.get("dataSourceId");
            String fileName = (String) params.get("fileName");
            String tableId = (String) params.get("tableId");

            // fileName 格式: {dataSourceId}/{tableName}.csv
            // 提取 tableName
            String tableName = fileName.replaceAll("\\.csv$", "").split("/")[1];

            log.info("开始解析 CSV 文件: dataSourceId={}, fileName={}, tableName={}", dataSourceId, fileName, tableName);

            context.updateProgress(50);

            // 解析 CSV 表结构（通过 DuckDB read_csv_auto 直接从 S3 读取推断）
            List<TableField> newFields = csvParseService.parseCsvSchema(dataSourceId, tableName, fileName);

            // 保存新增的字段记录（默认空列表，支持同步策略下的早期返回）
            List<TableField> fieldsToAdd;

            // 同步策略：如果是更新操作，删除 CSV 中不再存在的字段
            if (StringUtils.isNotBlank(tableId)) {
                Table existingTable = tableService.getById(tableId);
                if (existingTable != null) {
                    // 1.1 获取现有字段列表
                    List<TableField> existingFields = tableFieldService.list(
                            Wrappers.<TableField>lambdaQuery()
                                    .eq(TableField::getTableId, tableId));

                    // 1.2 删除 CSV 中不再存在的字段
                    List<String> deleteFieldIds = existingFields.stream()
                            .filter(ef -> newFields.stream()
                                    .noneMatch(f -> f.getFieldName().equals(ef.getFieldName())))
                            .map(TableField::getFieldId)
                            .toList();
                    if (!deleteFieldIds.isEmpty()) {
                        tableFieldService.removeByIds(deleteFieldIds);
                    }

                    // 1.3 跳过已存在的字段（同名不处理）
                    final List<String> finalExistFieldNames = existingFields.stream()
                            .map(TableField::getFieldName)
                            .toList();
                    fieldsToAdd = newFields.stream()
                            .filter(f -> !finalExistFieldNames.contains(f.getFieldName()))
                            .toList();

                    // 1.4 如果没有新字段需要添加，直接返回
                    if (fieldsToAdd.isEmpty()) {
                        context.updateProgress(100);
                        context.setResult("CSV 文件解析完成: tableId=" + tableId);
                        return;
                    }
                } else {
                    // 表不存在，说明可能被删除了，使用新字段列表
                    fieldsToAdd = newFields;
                }
            } else {
                // 新上传文件（无 tableId），所有字段都是新增的
                fieldsToAdd = newFields;
            }

            // 2. 获取或创建表记录
            Table table = StringUtils.isNotBlank(tableId) ? tableService.getById(tableId) : null;
            if (table == null) {
                table = new Table();
                table.setTableId(StringUtils.isNotBlank(tableId) ? tableId : CommonUtil.getUUIDV7String());
                table.setDataSourceId(dataSourceId);
                table.setTableName(tableName);
                table.setToUse(true);
                tableService.save(table);
            }

            // 3. 保存新增的字段记录
            for (TableField field : fieldsToAdd) {
                field.setTableId(table.getTableId());
                field.setToUse(true);
            }
            tableFieldService.saveBatch(fieldsToAdd);

            context.updateProgress(100);
            context.setResult("CSV 文件解析完成: tableId=" + table.getTableId());
        } catch (Exception e) {
            log.error("CSV 文件解析任务执行失败: taskId={}", taskId, e);
            throw new ServerException("CSV 文件解析任务执行失败: " + e.getMessage(), e);
        }
    }
}
