package cn.opensrcdevelop.ai.component;

import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.ai.service.csv.CsvDatasourceStorageService;
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

    private final CsvDatasourceStorageService csvStorageService;
    private final CsvParseService csvParseService;
    private final TableService tableService;
    private final TableFieldService tableFieldService;

    @Override
    public String getTaskType() {
        return AsyncTaskTypeEnum.CSV_PARSE.getCode();
    }

    @Override
    @SuppressWarnings("unchecked")
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

            context.updateProgress(30);

            // 从 S3 下载 CSV 文件
            String s3Path = "csv-datasource/" + fileName;
            byte[] csvData = csvStorageService.read(s3Path);
            log.info("CSV 文件下载成功: {} bytes", csvData.length);

            context.updateProgress(60);

            // 解析 CSV 表结构
            List<TableField> fields = csvParseService.parseCsvSchema(dataSourceId, tableName, s3Path);

            // 保存到数据库
            // 1. 如果是更新操作，先删除旧记录
            if (tableId != null && !tableId.isBlank()) {
                tableFieldService.remove(Wrappers.<TableField>lambdaQuery()
                        .eq(TableField::getTableId, tableId));
                Table oldTable = tableService.getById(tableId);
                if (oldTable != null) {
                    tableService.removeById(tableId);
                }
            }

            // 2. 创建新表记录
            Table table = new Table();
            table.setTableId(tableId != null ? tableId : CommonUtil.getUUIDV7String());
            table.setDataSourceId(dataSourceId);
            table.setTableName(tableName);
            table.setToUse(true);
            tableService.save(table);

            // 3. 保存字段记录
            for (TableField field : fields) {
                field.setTableId(table.getTableId());
            }
            tableFieldService.saveBatch(fields);

            context.updateProgress(100);

            log.info("CSV 文件解析完成: tableId={}, fieldCount={}", table.getTableId(), fields.size());

        } catch (Exception e) {
            log.error("CSV 文件解析任务执行失败: taskId={}", taskId, e);
            throw new ServerException("CSV 文件解析任务执行失败: " + e.getMessage(), e);
        }
    }
}
