package cn.opensrcdevelop.auth.biz.dto.user.attr;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "设置用户属性显示顺序")
@Data
public class SetUserAttrDisplaySeqRequestDto {

    @Schema(description = "属性 ID")
    @NotBlank
    private String id;

    @Schema(description = "显示顺序")
    @NotNull
    private Integer seq;
}
