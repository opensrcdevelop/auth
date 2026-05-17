package cn.opensrcdevelop.ai.service.csv.impl;

import cn.opensrcdevelop.ai.service.csv.CsvDatasourceStorageService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import jakarta.annotation.PostConstruct;
import java.net.URI;
import java.time.Duration;
import java.util.List;
import java.util.function.Consumer;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Strings;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.*;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest;
import software.amazon.awssdk.services.s3.presigner.model.UploadPartPresignRequest;

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

    @Value("${csv-ds.storage.s3.url-style:vhost}")
    private String s3UrlStyle;

    @Getter
    @Value("${csv-ds.storage.s3.url-expiration-minutes:10}")
    private int urlExpirationMinutes;

    @Value("${csv-ds.storage.s3.multipart-chunk-size-mb:5}")
    private int multipartChunkSizeMb;

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
        if (Strings.CI.equals("path", s3UrlStyle)) {
            s3Builder.forcePathStyle(true);
        }

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
                return endpoint + CommonConstants.SLASH + bucket + CommonConstants.SLASH + filePath;
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

    /**
     * 初始化分段上传
     *
     * @param key
     *            文件键
     * @return 上传ID
     */
    public String initiateMultipartUpload(String key) {
        try {
            CreateMultipartUploadRequest createRequest = CreateMultipartUploadRequest.builder()
                    .bucket(bucket)
                    .key(key)
                    .build();

            CreateMultipartUploadResponse response = s3Client.createMultipartUpload(createRequest);
            log.info("分段上传初始化成功: key={}, uploadId={}", key, response.uploadId());
            return response.uploadId();

        } catch (Exception e) {
            log.error("分段上传初始化失败: key={}", key, e);
            throw new ServerException("分段上传初始化失败", e);
        }
    }

    /**
     * 生成分片上传预签名URL
     *
     * @param key
     *            文件键
     * @param uploadId
     *            上传ID
     * @param partNumber
     *            分片编号（从1开始）
     * @return 预签名URL
     */
    public String generateUploadPartUrl(String key, String uploadId, int partNumber) {
        try {
            UploadPartPresignRequest presignRequest = UploadPartPresignRequest.builder()
                    .signatureDuration(Duration.ofMinutes(urlExpirationMinutes))
                    .uploadPartRequest(c -> c.bucket(bucket)
                            .key(key)
                            .uploadId(uploadId)
                            .partNumber(partNumber))
                    .build();

            return s3Presigner.presignUploadPart(presignRequest).url().toString();

        } catch (Exception e) {
            log.error("生成分片上传预签名URL失败: key={}, uploadId={}, partNumber={}", key, uploadId, partNumber, e);
            throw new ServerException("生成分片上传预签名URL失败", e);
        }
    }

    /**
     * 完成分段上传
     *
     * @param key
     *            文件键
     * @param uploadId
     *            上传ID
     * @param parts
     *            已上传的分片信息
     */
    public void completeMultipartUpload(String key, String uploadId, List<UploadedPart> parts) {
        try {
            CompleteMultipartUploadRequest completeRequest = CompleteMultipartUploadRequest.builder()
                    .bucket(bucket)
                    .key(key)
                    .uploadId(uploadId)
                    .multipartUpload(c -> c.parts(CompletedMultipartUpload.builder()
                            .parts(parts.stream()
                                    .map(p -> CompletedPart.builder()
                                            .eTag(p.etag())
                                            .partNumber(p.partNumber())
                                            .build())
                                    .toList())
                            .build().parts()))
                    .build();

            s3Client.completeMultipartUpload(completeRequest);
            log.info("分段上传完成: key={}, uploadId={}", key, uploadId);

        } catch (Exception e) {
            log.error("分段上传完成失败: key={}, uploadId={}", key, uploadId, e);
            throw new ServerException("分段上传完成失败", e);
        }
    }

    /**
     * 取消分段上传
     *
     * @param key
     *            文件键
     * @param uploadId
     *            上传ID
     */
    public void abortMultipartUpload(String key, String uploadId) {
        try {
            AbortMultipartUploadRequest abortRequest = AbortMultipartUploadRequest.builder()
                    .bucket(bucket)
                    .key(key)
                    .uploadId(uploadId)
                    .build();

            s3Client.abortMultipartUpload(abortRequest);
            log.info("分段上传已取消: key={}, uploadId={}", key, uploadId);

        } catch (Exception e) {
            log.error("取消分段上传失败: key={}, uploadId={}", key, uploadId, e);
            throw new ServerException("取消分段上传失败", e);
        }
    }

    /**
     * 获取分片大小（字节）
     */
    public int getChunkSizeBytes() {
        return multipartChunkSizeMb * 1024 * 1024;
    }

    /**
     * 已上传分片信息
     */
    public record UploadedPart(int partNumber, String etag) {
    }
}
