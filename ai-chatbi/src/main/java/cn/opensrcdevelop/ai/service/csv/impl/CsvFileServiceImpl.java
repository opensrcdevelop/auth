package cn.opensrcdevelop.ai.service.csv.impl;

import cn.opensrcdevelop.ai.component.CsvParseAsyncTaskExecutor;
import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.dto.CsvFileResponseDto;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.ai.service.csv.CsvDatasourceStorageService;
import cn.opensrcdevelop.ai.service.csv.CsvFileService;
import cn.opensrcdevelop.auth.biz.constants.AsyncTaskTypeEnum;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskSchedulerService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

/**
 * CSV 文件服务实现
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class CsvFileServiceImpl implements CsvFileService {

    private final CsvDatasourceStorageService csvStorageService;
    private final AsyncTaskSchedulerService asyncTaskSchedulerService;
    private final TableService tableService;
    private final TableFieldService tableFieldService;

    @Override
    public String uploadCsv(MultipartFile file, String dataSourceId) {
        // 1. 验证文件名
        String originalFilename = file.getOriginalFilename();
        if (originalFilename == null || !originalFilename.endsWith(".csv")) {
            throw new BizException(MessageConstants.AI_CSV_MSG_1000);
        }

        if (originalFilename.contains("..")) {
            throw new BizException(MessageConstants.AI_CSV_MSG_1001);
        }

        // 2. 上传到 S3
        String tableName = originalFilename.replaceAll("\\.csv$", "");
        String fileName = dataSourceId + CommonConstants.SLASH + tableName + ".csv";
        try {
            csvStorageService.store(file.getBytes(), fileName);
        } catch (IOException e) {
            throw new ServerException("CSV 文件上传失败", e);
        }

        // 3. 检查是否已存在同名表
        Table existingTable = tableService.getOne(
                Wrappers.<Table>lambdaQuery()
                        .eq(Table::getDataSourceId, dataSourceId)
                        .eq(Table::getTableName, tableName));

        // 4. 提交异步解析任务
        Map<String, Object> params = new HashMap<>();
        params.put("dataSourceId", dataSourceId);
        params.put("fileName", fileName);
        if (existingTable != null) {
            params.put("tableId", existingTable.getTableId());
        }

        return asyncTaskSchedulerService.submitTask(
                AsyncTaskTypeEnum.CSV_PARSE.getCode(),
                CsvParseAsyncTaskExecutor.TASK_NAME,
                params,
                AuthUtil.getCurrentUserId());
    }

    @Override
    public List<CsvFileResponseDto> listCsvFiles(String dataSourceId) {
        return tableService.listCsvFiles(dataSourceId);
    }

    @Override
    public void deleteCsv(String tableId) {
        // 1. 获取表信息
        Table table = tableService.getById(tableId);
        if (table == null) {
            return;
        }

        // 2. 删除 S3 文件
        String s3Path = table.getDataSourceId() + CommonConstants.SLASH
                + table.getTableName().replaceAll("\\.csv$", "") + ".csv";
        csvStorageService.delete(s3Path);

        // 3. 删除 t_table_field 记录
        tableFieldService.remove(Wrappers.<TableField>lambdaQuery()
                .eq(TableField::getTableId, tableId));

        // 4. 删除 t_table 记录
        tableService.removeById(tableId);
    }
}
