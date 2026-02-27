package cn.opensrcdevelop.auth.biz.service.user.excel;

import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.enums.AsyncTaskType;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutor;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.fasterxml.jackson.core.type.TypeReference;
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

    public static final String TASK_NAME = "用户数据导出";
    public static final String PARAM_KEY_FILTERS = "filters";
    public static final String PARAM_KEY_EXPORT_ALL = "exportAll";
    public static final String PARAM_KEY_USER_IDS = "userIds";
    private static final String RESULT_FILE_NAME_FORMAT = "用户导出_%s.xlsx";

    private final UserExcelService userExcelService;

    @Override
    public String getTaskType() {
        return AsyncTaskType.USER_EXPORT.getCode();
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

            // 提取导出参数
            List<DataFilterDto> filters = CommonUtil.convertObj(params.get(PARAM_KEY_FILTERS),
                    new TypeReference<List<DataFilterDto>>() {
                    });
            boolean exportAll = params.get(PARAM_KEY_EXPORT_ALL) == null || (Boolean) params.get(PARAM_KEY_EXPORT_ALL);
            List<String> userIds = params.get(PARAM_KEY_USER_IDS) != null
                    ? (List<String>) params.get(PARAM_KEY_USER_IDS)
                    : null;

            context.updateProgress(30);

            // 执行导出
            byte[] exportData = userExcelService.exportUsers(filters, exportAll, userIds);

            context.updateProgress(80);

            // 存储结果文件
            String timestamp = LocalDateTime.now()
                    .format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS_COMPACT));
            String fileName = RESULT_FILE_NAME_FORMAT.formatted(timestamp);
            context.storeResultFile(exportData, fileName);

            context.updateProgress(100);

            log.info("用户导出任务完成: taskId={}, fileName={}", taskId, fileName);

        } catch (Exception e) {
            log.error("用户导出任务执行失败: taskId={}", taskId, e);
            throw new ServerException("用户导出任务执行失败: " + e.getMessage(), e);
        }
    }
}
