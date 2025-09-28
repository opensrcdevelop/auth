package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

@Schema(description = "批量更新表字段请求")
@Data
public class BatchUpdateTableFieldRequestDto {

    @Schema(description = "表字段列表")
    @NotEmpty
    public List<@Valid TableFieldRequestDto> list;
}
