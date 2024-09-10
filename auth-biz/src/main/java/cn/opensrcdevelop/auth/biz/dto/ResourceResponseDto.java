package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "资源响应")
@Data
public class ResourceResponseDto {

    @Schema(description = "资源ID")
    private String id;

    @Schema(description = "资源名称")
    private String name;

    @Schema(description = "资源标识")
    private String code;

    @Schema(description = "资源描述")
    private String desc;

    @Schema(description = "API URL 标识")
    private String api;

    @Schema(description = "资源组")
    private ResourceGroupResponseDto resourceGroup;
}
