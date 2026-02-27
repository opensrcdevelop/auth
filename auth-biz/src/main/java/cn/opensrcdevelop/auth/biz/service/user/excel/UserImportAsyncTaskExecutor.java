package cn.opensrcdevelop.auth.biz.service.user.excel;

import cn.opensrcdevelop.auth.biz.enums.AsyncTaskType;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutor;
import cn.opensrcdevelop.auth.biz.service.asynctask.storage.StorageService;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * 用户导入异步任务执行器
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UserImportAsyncTaskExecutor implements AsyncTaskExecutor {

    public static final String TASK_NAME = "用户数据导入";
    public static final String PARAM_KEY_FILE_PATH = "filePath";
    public static final String PARAM_KEY_FILE_NAME = "fileName";

    private final UserExcelService userExcelService;
    private final StorageService storageService;

    @Override
    public String getTaskType() {
        return AsyncTaskType.USER_IMPORT.getCode();
    }

    @Override
    public void execute(String taskId, String taskParams, TaskExecutionContext context) {
        try {
            context.updateProgress(10);

            // 解析任务参数
            Map<String, Object> params = CommonUtil.nonJdkDeserializeObject(taskParams,
                    new TypeReference<Map<String, Object>>() {
                    });

            // 获取上传的文件信息
            String filePath = (String) params.get(PARAM_KEY_FILE_PATH);
            String fileName = (String) params.get(PARAM_KEY_FILE_NAME);

            context.updateProgress(30);

            if (filePath == null || fileName == null) {
                throw new IllegalArgumentException("任务参数缺少文件信息");
            }

            // 从存储服务读取文件
            byte[] fileData = storageService.read(filePath);
            log.info("从 {} 读取导入文件: {}, 大小: {} bytes", filePath, fileName, fileData.length);

            context.updateProgress(50);

            // 执行导入
            var importResult = userExcelService.importUsers(fileData);

            context.updateProgress(90);

            // 设置结果
            String resultJson = CommonUtil.nonJdkSerializeObject(importResult);
            context.setResult(resultJson);

            context.updateProgress(100);

            log.info("用户导入任务完成: taskId={}, 创建: {}, 更新: {}, 删除: {}",
                    taskId, importResult.getCreatedCount(),
                    importResult.getUpdatedCount(), importResult.getDeletedCount());

        } catch (Exception e) {
            log.error("用户导入任务执行失败: taskId={}", taskId, e);
            throw new ServerException("用户导入任务执行失败: " + e.getMessage(), e);
        }
    }
}
