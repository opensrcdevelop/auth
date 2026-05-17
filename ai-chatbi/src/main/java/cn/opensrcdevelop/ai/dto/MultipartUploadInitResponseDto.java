package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 分片上传初始化响应
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "分片上传初始化响应")
public class MultipartUploadInitResponseDto {

    @Schema(description = "文件键（S3路径）")
    private String key;

    @Schema(description = "上传ID")
    private String uploadId;

    @Schema(description = "分片大小（字节）")
    private int chunkSize;

    @Schema(description = "预签名URL有效期（分钟）")
    private int urlExpirationMinutes;
}
