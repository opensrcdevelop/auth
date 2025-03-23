package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "邮件服务配置 请求/响应")
@Data
public class MailServiceConfigDto {

    @Schema(description = "SMTP 服务器地址")
    @NotBlank
    private String host;

    @Schema(description = "SMTP 服务器端口")
    @NotNull
    private Integer port;

    @Schema(description = "用户名")
    @NotBlank
    private String username;

    @Schema(description = "密码")
    @NotBlank
    private String password;

    @Schema(description = "是否启用 SSL")
    @NotNull
    private Boolean sslEnable;
}
