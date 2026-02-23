package cn.opensrcdevelop.auth.biz.service.asynctask.storage.impl;

import cn.opensrcdevelop.auth.biz.service.asynctask.storage.StorageService;
import cn.opensrcdevelop.common.exception.ServerException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

/**
 * 本地存储服务实现
 */
@Slf4j
@Service
@ConditionalOnProperty(name = "async-task.storage.type", havingValue = "local", matchIfMissing = true)
public class LocalStorageService implements StorageService {

    @Value("${async-task.storage.local.base-path:/tmp/async-task}")
    private String basePath;

    @Value("${async-task.storage.local.base-url:}")
    private String baseUrl;

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");

    @Override
    public String store(byte[] data, String fileName) {
        try {
            // 1. 按日期创建子目录
            String datePath = LocalDate.now().format(DATE_FORMATTER);
            Path directory = Paths.get(basePath, datePath);

            // 2. 创建目录
            Files.createDirectories(directory);

            // 3. 生成唯一文件名
            String uniqueFileName = UUID.randomUUID() + "_" + fileName;
            Path filePath = directory.resolve(uniqueFileName);

            // 4. 写入文件
            Files.write(filePath, data);

            // 5. 返回相对路径（包含日期目录）
            String relativePath = Paths.get(datePath, uniqueFileName).toString();
            log.info("文件存储成功: path={}", relativePath);

            return relativePath;
        } catch (IOException e) {
            log.error("文件存储失败: fileName={}", fileName, e);
            throw new ServerException("文件存储失败", e);
        }
    }

    @Override
    public byte[] read(String filePath) {
        try {
            Path path = Paths.get(basePath, filePath);
            return Files.readAllBytes(path);
        } catch (IOException e) {
            log.error("文件读取失败: filePath={}", filePath, e);
            throw new ServerException("文件读取失败", e);
        }
    }

    @Override
    public void delete(String filePath) {
        try {
            Path path = Paths.get(basePath, filePath);
            Files.deleteIfExists(path);
            log.info("文件删除成功: filePath={}", filePath);
        } catch (IOException e) {
            log.error("文件删除失败: filePath={}", filePath, e);
            throw new ServerException("文件删除失败", e);
        }
    }

    @Override
    public String getUrl(String filePath) {
        if (StringUtils.isNotBlank(baseUrl)) {
            return Paths.get(basePath, filePath).toString();
        }
        // 如果没有配置 baseUrl，返回相对路径
        return filePath;
    }

    @Override
    public String getType() {
        return TYPE_LOCAL;
    }
}
