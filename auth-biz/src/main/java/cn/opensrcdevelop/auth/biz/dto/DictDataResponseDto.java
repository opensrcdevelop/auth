package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Schema(description = "字典数据响应")
@Data
public class DictDataResponseDto implements Serializable {

    @Serial
    private static final long serialVersionUID = 2865788407458441780L;

    @Schema(description = "字典ID")
    private String dictId;

    @Schema(description = "字典数据ID")
    private String id;

    @Schema(description = "字典数据标签")
    private String label;

    @Schema(description = "字典数据值")
    private String value;

    @Schema(description = "状态：true - 启用、false - 禁用")
    private Boolean enable;

    @Schema(description = "显示顺序")
    private Integer displaySeq;
}
