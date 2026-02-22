package cn.opensrcdevelop.auth.biz.service.user.excel;

import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.enums.AsyncTaskType;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutor;
import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * 用户导出异步任务执行器
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UserExportAsyncTaskExecutor implements AsyncTaskExecutor {

    private final UserExcelService userExcelService;
    private final ObjectMapper objectMapper;

    @Override
    public String getTaskType() {
        return AsyncTaskType.USER_EXPORT.getCode();
    }

    @Override
    public void execute(String taskId, String taskParams, TaskExecutionContext context) {
        try {
            context.updateProgress(10);

            // 解析任务参数
            Map<String, Object> params = objectMapper.readValue(taskParams,
                    new TypeReference<Map<String, Object>>() {
                    });

            // 提取导出参数
            @SuppressWarnings("unchecked")
            List<DataFilterDto> filters = objectMapper.convertValue(
                    params.get("filters"),
                    new TypeReference<List<DataFilterDto>>() {
                    });
            boolean exportAll = params.get("exportAll") != null ? (Boolean) params.get("exportAll") : true;
            @SuppressWarnings("unchecked")
            List<String> userIds = params.get("userIds") != null ? (List<String>) params.get("userIds") : null;

            context.updateProgress(30);

            // 执行导出
            byte[] exportData = userExcelService.exportUsers(filters, exportAll, userIds);

            context.updateProgress(80);

            // 存储结果文件
            String timestamp = LocalDateTime.now()
                    .format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS_COMPACT));
            String fileName = "用户导出_" + timestamp + ".xlsx";
            String filePath = context.storeResultFile(exportData, fileName);

            context.updateProgress(100);

            // 设置结果
            context.setResult("{\"message\": \"导出成功\", \"fileName\": \"" + fileName + "\"}");

            log.info("用户导出任务完成: taskId={}, fileName={}", taskId, fileName);

        } catch (Exception e) {
            log.error("用户导出任务执行失败: taskId={}", taskId, e);
            throw new RuntimeException("用户导出任务执行失败: " + e.getMessage(), e);
        }
    }
}
