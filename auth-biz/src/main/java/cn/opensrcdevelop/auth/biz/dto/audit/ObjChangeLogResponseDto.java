package cn.opensrcdevelop.auth.biz.dto.audit;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

@Schema(description = "对象变更日志响应")
@Data
public class ObjChangeLogResponseDto {

    @Schema(description = "变更")
    private List<String> changes;
}
