package cn.opensrcdevelop.ai.service.csv.impl;

import cn.opensrcdevelop.ai.component.CsvParseAsyncTaskExecutor;
import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.dto.CsvFileResponseDto;
import cn.opensrcdevelop.ai.dto.MultipartUploadCompleteRequestDto;
import cn.opensrcdevelop.ai.dto.MultipartUploadInitRequestDto;
import cn.opensrcdevelop.ai.dto.MultipartUploadInitResponseDto;
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
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

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
    public MultipartUploadInitResponseDto initMultipartUpload(MultipartUploadInitRequestDto request) {
        // 1. 验证文件名
        String originalFilename = request.getFilename();
        if (originalFilename == null || !originalFilename.endsWith(".csv")) {
            throw new BizException(MessageConstants.AI_CSV_MSG_1000);
        }

        if (originalFilename.contains("..")) {
            throw new BizException(MessageConstants.AI_CSV_MSG_1001);
        }

        // 2. 生成文件键
        String tableName = originalFilename.replaceAll("\\.csv$", "");
        String key = request.getDataSourceId() + CommonConstants.SLASH + tableName + ".csv";

        // 3. 初始化分片上传
        CsvDatasourceStorageServiceImpl storageService = (CsvDatasourceStorageServiceImpl) csvStorageService;
        String uploadId = storageService.initiateMultipartUpload(key);

        // 4. 构建响应
        return MultipartUploadInitResponseDto.builder()
                .key(key)
                .uploadId(uploadId)
                .chunkSize(storageService.getChunkSizeBytes())
                .urlExpirationMinutes(storageService.getUrlExpirationMinutes())
                .build();
    }

    @Override
    public String completeMultipartUpload(MultipartUploadCompleteRequestDto request) {
        // 1. 转换为存储服务需要的格式
        List<CsvDatasourceStorageServiceImpl.UploadedPart> parts = request.getParts().stream()
                .map(p -> new CsvDatasourceStorageServiceImpl.UploadedPart(p.getPartNumber(), p.getEtag()))
                .toList();

        // 2. 完成分片上传
        CsvDatasourceStorageServiceImpl storageService = (CsvDatasourceStorageServiceImpl) csvStorageService;
        storageService.completeMultipartUpload(request.getKey(), request.getUploadId(), parts);

        // 3. 提交异步解析任务
        String tableName = request.getOriginalFilename().replaceAll("\\.csv$", "");
        String fileName = request.getKey();

        // 检查是否已存在同名表
        Table existingTable = tableService.getOne(
                Wrappers.<Table>lambdaQuery()
                        .eq(Table::getDataSourceId, request.getDataSourceId())
                        .eq(Table::getTableName, tableName));

        Map<String, Object> params = new HashMap<>();
        params.put("dataSourceId", request.getDataSourceId());
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
    public String generateUploadPartUrl(String key, String uploadId, int partNumber) {
        CsvDatasourceStorageServiceImpl storageService = (CsvDatasourceStorageServiceImpl) csvStorageService;
        return storageService.generateUploadPartUrl(key, uploadId, partNumber);
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
