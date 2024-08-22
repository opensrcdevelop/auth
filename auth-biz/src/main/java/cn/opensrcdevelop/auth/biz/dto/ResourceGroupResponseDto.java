package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "资源组响应")
@Data
public class ResourceGroupResponseDto {

    @Schema(description = "资源组ID")
    private String id;

    @Schema(description = "资源组名称")
    private String name;

    @Schema(description = "资源组标识")
    private String code;

    @Schema(description = "资源组描述")
    private String desc;
}
