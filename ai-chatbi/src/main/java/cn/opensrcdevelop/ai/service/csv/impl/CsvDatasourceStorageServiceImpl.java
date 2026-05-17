package cn.opensrcdevelop.ai.service.csv.impl;

import cn.opensrcdevelop.ai.service.csv.CsvDatasourceStorageService;
import cn.opensrcdevelop.common.exception.ServerException;
import jakarta.annotation.PostConstruct;
import java.net.URI;
import java.time.Duration;
import java.util.List;
import java.util.function.Consumer;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.core.BytesWrapper;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.*;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest;

/**
 * CSV 数据源 S3 存储服务实现
 * <p>
 * 使用 csv-ds.storage.s3.* 配置，支持 AWS S3、MinIO、阿里云 OSS 等兼容 S3 协议的对象存储
 * </p>
 */
@Slf4j
@Service
public class CsvDatasourceStorageServiceImpl implements CsvDatasourceStorageService {

    @Value("${csv-ds.storage.s3.endpoint:}")
    private String endpoint;

    @Value("${csv-ds.storage.s3.region:}")
    private String region;

    @Value("${csv-ds.storage.s3.access-key:}")
    private String accessKey;

    @Value("${csv-ds.storage.s3.secret-key:}")
    private String secretKey;

    @Value("${csv-ds.storage.s3.bucket:}")
    private String bucket;

    @Value("${csv-ds.storage.s3.path-style-access:false}")
    private boolean pathStyleAccess;

    @Value("${csv-ds.storage.s3.url-expiration-minutes:10}")
    private int urlExpirationMinutes;

    private S3Client s3Client;
    private S3Presigner s3Presigner;

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

        log.info("CsvDatasourceStorageService 初始化完成: endpoint={}, bucket={}", endpoint, bucket);
    }

    @Override
    public void store(byte[] data, String key) {
        try {
            // 上传到 S3
            PutObjectRequest putRequest = PutObjectRequest.builder()
                    .bucket(bucket)
                    .key(key)
                    .build();

            s3Client.putObject(putRequest, RequestBody.fromBytes(data));

            log.info("CSV 文件上传成功: bucket={}, key={}", bucket, key);

        } catch (Exception e) {
            log.error("CSV 文件上传失败: key={}", key, e);
            throw new ServerException("CSV 文件上传失败", e);
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
            log.error("CSV 文件读取失败: filePath={}", filePath, e);
            throw new ServerException("CSV 文件读取失败", e);
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
            log.info("CSV 文件删除成功: bucket={}, key={}", bucket, filePath);
        } catch (Exception e) {
            log.error("CSV 文件删除失败: filePath={}", filePath, e);
            throw new ServerException("CSV 文件删除失败", e);
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
    public List<String> list(String prefix) {
        try {
            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(bucket)
                    .prefix(prefix)
                    .build();

            return s3Client.listObjectsV2Paginator(listRequest)
                    .contents()
                    .stream()
                    .map(S3Object::key)
                    .toList();
        } catch (Exception e) {
            log.error("CSV 文件列表获取失败: prefix={}", prefix, e);
            throw new ServerException("CSV 文件列表获取失败", e);
        }
    }
}
