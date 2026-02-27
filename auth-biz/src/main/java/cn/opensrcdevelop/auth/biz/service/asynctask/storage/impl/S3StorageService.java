package cn.opensrcdevelop.auth.biz.service.asynctask.storage.impl;

import cn.opensrcdevelop.auth.biz.service.asynctask.storage.StorageService;
import cn.opensrcdevelop.common.exception.ServerException;
import jakarta.annotation.PostConstruct;
import java.net.URI;
import java.time.Duration;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.UUID;
import java.util.function.Consumer;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.core.BytesWrapper;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.DeleteObjectRequest;
import software.amazon.awssdk.services.s3.model.GetObjectRequest;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest;

/**
 * S3 存储服务实现，支持 AWS S3、MinIO、阿里云 OSS 等兼容 S3 协议的对象存储
 */
@Slf4j
@Service
@ConditionalOnProperty(name = "async-task.storage.type", havingValue = "s3")
public class S3StorageService implements StorageService {

    @Value("${async-task.storage.s3.endpoint:}")
    private String endpoint;

    @Value("${async-task.storage.s3.region:}")
    private String region;

    @Value("${async-task.storage.s3.access-key:}")
    private String accessKey;

    @Value("${async-task.storage.s3.secret-key:}")
    private String secretKey;

    @Value("${async-task.storage.s3.bucket:}")
    private String bucket;

    @Value("${async-task.storage.s3.path-style-access:false}")
    private boolean pathStyleAccess;

    @Value("${async-task.storage.s3.url-expiration-minutes:10}")
    private int urlExpirationMinutes;

    private S3Client s3Client;
    private S3Presigner s3Presigner;

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");

    @PostConstruct
    public void init() {
        var s3Builder = S3Client.builder();

        // 配置端点和认证
        if (StringUtils.isNotBlank(endpoint)) {
            s3Builder.endpointOverride(URI.create(endpoint));
        }
        if (StringUtils.isNotBlank(accessKey) && StringUtils.isNotBlank(secretKey)) {
            s3Builder.credentialsProvider(StaticCredentialsProvider.create(
                    AwsBasicCredentials.create(accessKey, secretKey)));
        }
        if (StringUtils.isNotBlank(region)) {
            s3Builder.region(Region.of(region));
        }

        // 配置路径风格
        s3Builder.forcePathStyle(pathStyleAccess);

        s3Client = s3Builder.build();

        // 初始化 Presigner
        var presignerBuilder = S3Presigner.builder();
        if (StringUtils.isNotBlank(endpoint)) {
            presignerBuilder.endpointOverride(URI.create(endpoint));
        }
        if (StringUtils.isNotBlank(accessKey) && StringUtils.isNotBlank(secretKey)) {
            presignerBuilder.credentialsProvider(StaticCredentialsProvider.create(
                    AwsBasicCredentials.create(accessKey, secretKey)));
        }
        if (StringUtils.isNotBlank(region)) {
            presignerBuilder.region(Region.of(region));
        }

        s3Presigner = presignerBuilder.build();

        log.info("S3 存储服务初始化完成: endpoint={}, bucket={}", endpoint, bucket);
    }

    @Override
    public String store(byte[] data, String fileName) {
        try {
            // 1. 按日期创建子目录
            String datePath = LocalDate.now().format(DATE_FORMATTER);

            // 2. 生成唯一文件名
            String uniqueFileName = UUID.randomUUID() + "_" + fileName;
            String objectKey = datePath + "/" + uniqueFileName;

            // 3. 上传到 S3
            PutObjectRequest putRequest = PutObjectRequest.builder()
                    .bucket(bucket)
                    .key(objectKey)
                    .build();

            s3Client.putObject(putRequest, RequestBody.fromBytes(data));

            log.info("文件上传成功: bucket={}, key={}", bucket, objectKey);

            // 4. 返回相对路径
            return objectKey;
        } catch (Exception e) {
            log.error("文件上传失败: fileName={}", fileName, e);
            throw new ServerException("文件上传失败", e);
        }
    }

    @Override
    public byte[] read(String filePath) {
        try {
            // 使用 Consumer Builder 模式
            GetObjectRequest getRequest = GetObjectRequest.builder()
                    .bucket(bucket)
                    .key(filePath)
                    .build();

            // 直接获取字节数组
            BytesWrapper bytesWrapper = s3Client.getObjectAsBytes(getRequest);
            return bytesWrapper.asByteArrayUnsafe();
        } catch (Exception e) {
            log.error("文件读取失败: filePath={}", filePath, e);
            throw new ServerException("文件读取失败", e);
        }
    }

    @Override
    public void delete(String filePath) {
        try {
            DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                    .bucket(bucket)
                    .key(filePath)
                    .build();

            s3Client.deleteObject(deleteRequest);
            log.info("文件删除成功: bucket={}, key={}", bucket, filePath);
        } catch (Exception e) {
            log.error("文件删除失败: filePath={}", filePath, e);
            throw new ServerException("文件删除失败", e);
        }
    }

    @Override
    public String getUrl(String filePath) {
        try {
            // 生成预签名 URL
            Consumer<GetObjectRequest.Builder> requestConsumer = r -> r.bucket(bucket).key(filePath);
            GetObjectPresignRequest presignRequest = GetObjectPresignRequest.builder()
                    .signatureDuration(Duration.ofMinutes(urlExpirationMinutes))
                    .getObjectRequest(requestConsumer)
                    .build();

            return s3Presigner.presignGetObject(presignRequest).url().toString();
        } catch (Exception e) {
            log.warn("生成预签名URL失败，使用默认方式: filePath={}", filePath, e);
            // 如果生成失败，尝试返回直接访问路径
            if (StringUtils.isNotBlank(endpoint)) {
                return endpoint + "/" + bucket + "/" + filePath;
            }
            return filePath;
        }
    }

    @Override
    public String getType() {
        return TYPE_S3;
    }
}
