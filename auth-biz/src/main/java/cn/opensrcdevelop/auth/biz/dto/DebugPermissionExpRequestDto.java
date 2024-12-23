package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.Map;

@Schema(description = "调试权限表达式请求")
@Data
public class DebugPermissionExpRequestDto {

    @Schema(description = "权限表达式ID")
    @NotBlank
    private String expressionId;

    @Schema(description = "上下文")
    private Map<String, Object> context;
}
