package cn.opensrcdevelop.auth.biz.dto.permission;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "验证权限响应")
@Builder
@Setter
@Getter
public class VerifyPermissionResponseDto {

    @Schema(description = "权限")
    private String permission;

    @Schema(description = "是否允许")
    private Boolean allow;
}
