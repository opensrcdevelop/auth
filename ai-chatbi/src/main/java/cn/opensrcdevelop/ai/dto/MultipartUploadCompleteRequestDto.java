package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 分片上传完成请求
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "分片上传完成请求")
public class MultipartUploadCompleteRequestDto {

    @NotBlank
    @Schema(description = "文件键（S3路径）")
    private String key;

    @NotBlank
    @Schema(description = "上传ID")
    private String uploadId;

    @NotEmpty
    @Schema(description = "已上传的分片列表")
    private List<UploadedPartDto> parts;

    @NotBlank
    @Schema(description = "数据源ID")
    private String dataSourceId;

    @NotBlank
    @Schema(description = "原始文件名")
    private String originalFilename;

    /**
     * 已上传分片信息
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class UploadedPartDto {

        @NotNull
        @Schema(description = "分片编号（从1开始）")
        private Integer partNumber;

        @NotBlank
        @Schema(description = "ETag")
        private String etag;
    }
}
