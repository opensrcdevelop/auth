package cn.opensrcdevelop.auth.biz.service.user.excel;

import cn.opensrcdevelop.auth.biz.enums.AsyncTaskType;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutor;
import cn.opensrcdevelop.auth.biz.service.asynctask.storage.StorageService;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

/**
 * 用户导入异步任务执行器
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UserImportAsyncTaskExecutor implements AsyncTaskExecutor {

    private final UserExcelService userExcelService;
    private final StorageService storageService;
    private final ObjectMapper objectMapper;

    @Override
    public String getTaskType() {
        return AsyncTaskType.USER_IMPORT.getCode();
    }

    @Override
    public void execute(String taskId, String taskParams, TaskExecutionContext context) {
        try {
            context.updateProgress(10);

            // 解析任务参数
            Map<String, Object> params = objectMapper.readValue(taskParams,
                    new TypeReference<Map<String, Object>>() {
                    });

            // 获取上传的文件信息
            String filePath = (String) params.get("filePath");
            String fileName = (String) params.get("fileName");

            context.updateProgress(30);

            if (filePath == null || fileName == null) {
                throw new IllegalArgumentException("任务参数缺少文件信息");
            }

            // 从存储服务读取文件
            byte[] fileData = storageService.read(filePath);
            log.info("从 {} 读取导入文件: {}, 大小: {} bytes", filePath, fileName, fileData.length);

            context.updateProgress(50);

            // 转换为 MultipartFile 并执行导入
            MultipartFile multipartFile = new ByteArrayMultipartFile(fileData, fileName);

            // 执行导入
            var importResult = userExcelService.importUsers(multipartFile);

            context.updateProgress(90);

            // 设置结果
            String resultJson = objectMapper.writeValueAsString(importResult);
            context.setResult(resultJson);

            context.updateProgress(100);

            log.info("用户导入任务完成: taskId={}, 创建: {}, 更新: {}, 删除: {}",
                    taskId, importResult.getCreatedCount(),
                    importResult.getUpdatedCount(), importResult.getDeletedCount());

        } catch (Exception e) {
            log.error("用户导入任务执行失败: taskId={}", taskId, e);
            throw new RuntimeException("用户导入任务执行失败: " + e.getMessage(), e);
        }
    }

    /**
     * 字节数组到 MultipartFile 的简单转换
     */
    private static class ByteArrayMultipartFile implements MultipartFile {
        private final byte[] data;
        private final String filename;

        public ByteArrayMultipartFile(byte[] data, String filename) {
            this.data = data;
            this.filename = filename;
        }

        @Override
        public String getName() {
            return "file";
        }

        @Override
        public String getOriginalFilename() {
            return filename;
        }

        @Override
        public String getContentType() {
            return "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
        }

        @Override
        public boolean isEmpty() {
            return data == null || data.length == 0;
        }

        @Override
        public long getSize() {
            return data.length;
        }

        @Override
        public byte[] getBytes() {
            return data;
        }

        @Override
        public InputStream getInputStream() {
            return new ByteArrayInputStream(data);
        }

        @Override
        public void transferTo(File dest) throws IOException {
            Files.write(dest.toPath(), data);
        }
    }
}
