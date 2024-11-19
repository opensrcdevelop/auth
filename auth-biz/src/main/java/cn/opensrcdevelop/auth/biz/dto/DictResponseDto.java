package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "字典响应")
@Data
public class DictResponseDto {

    @Schema(description = "字典ID")
    private String id;

    @Schema(description = "字典名称")
    private String name;

    @Schema(description = "字典标识")
    private String code;

    @Schema(description = "字典描述")
    private String desc;

    @Schema(description = "字典数据条数")
    private long dataCnt;
}
