package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "用户属性响应")
@Data
public class UserAttrResponseDto {

    @Schema(description = "属性 ID")
    private String id;

    @Schema(description = "属性键")
    private String key;

    @Schema(description = "属性名")
    private String name;

    @Schema(description = "属性数据类型")
    private String dataType;

    @Schema(description = "属性值")
    private Object value;

    @Schema(description = "扩展属性标记")
    private Boolean extFlg;

    @Schema(description = "是否在用户列表显示")
    private Boolean userLstDisplay;

    @Schema(description = "显示顺序")
    private Integer displaySeq;

    @Schema(description = "显示宽度")
    private Integer displayWidth;
}
