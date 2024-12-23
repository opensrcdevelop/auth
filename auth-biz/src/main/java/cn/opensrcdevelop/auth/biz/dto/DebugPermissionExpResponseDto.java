package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "调试权限表达式响应")
@Data
public class DebugPermissionExpResponseDto {

    @Schema(description = "是否成功")
    private Boolean isSuccess;

    @Schema(description = "执行结果")
    private Object executeRes;
}
