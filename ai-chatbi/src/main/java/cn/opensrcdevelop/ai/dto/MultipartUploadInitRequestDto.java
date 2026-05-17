package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Positive;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 分片上传初始化请求
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "分片上传初始化请求")
public class MultipartUploadInitRequestDto {

    @NotBlank
    @Schema(description = "数据源ID")
    private String dataSourceId;

    @NotBlank
    @Schema(description = "文件名（不含路径）")
    private String filename;

    @Positive
    @Schema(description = "文件大小（字节）")
    private long fileSize;
}
