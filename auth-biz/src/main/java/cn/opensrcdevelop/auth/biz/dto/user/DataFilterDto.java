package cn.opensrcdevelop.auth.biz.dto.user;

import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "过滤请求/响应")
@Data
public class DataFilterDto {

    @Schema(description = "属性键")
    @NotBlank
    private String key;

    @Schema(description = "属性数据类型")
    @NotBlank
    @EnumValue({ "STRING", "BOOLEAN", "NUMBER", "DATETIME", "DATE", "DICT" })
    private String dataType;

    @Schema(description = "属性值")
    @NotNull
    private Object value;

    @Schema(description = "过滤类型")
    @NotBlank
    @EnumValue({ "EQ", "NE", "LIKE", "NOT_LIKE", "IN", "NOT_IN", "GT", "GE", "LT", "LE" })
    private String filterType;

    @Schema(description = "扩展属性标记")
    @NotNull
    private Boolean extFlg;
}
