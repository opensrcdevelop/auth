package cn.opensrcdevelop.auth.biz.dto.system.mail;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@EntityName("邮件服务配置")
@Schema(description = "邮件服务配置 请求/响应")
@Data
public class MailServiceConfigDto {

    @PropertyName("SMTP 服务器地址")
    @Schema(description = "SMTP 服务器地址")
    @NotBlank
    private String host;

    @PropertyName("SMTP 服务器端口")
    @Schema(description = "SMTP 服务器端口")
    @NotNull
    private Integer port;

    @PropertyName("用户名")
    @Schema(description = "用户名")
    @NotBlank
    private String username;

    @PropertyName("密码")
    @Schema(description = "密码")
    @NotBlank
    private String password;

    @PropertyName("是否启用 SSL")
    @Schema(description = "是否启用 SSL")
    @NotNull
    private Boolean sslEnable;
}
