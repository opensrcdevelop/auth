package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import java.util.List;
import lombok.Data;

@Schema(description = "批量更新表请求")
@Data
public class BatchUpdateTableRequestDto {

    @Schema(description = "表列表")
    @NotEmpty
    public List<@Valid TableRequestDto> list;
}
