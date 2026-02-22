package cn.opensrcdevelop.auth.biz.dto.user.attr.dict;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
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

    @Schema(description = "父字典ID")
    private String parentId;

    @Schema(description = "唯一键")
    private String uniqueKey;

    @Schema(description = "字典层级")
    private Integer level;

    @Schema(description = "子字典集合")
    private List<DictResponseDto> children;
}
