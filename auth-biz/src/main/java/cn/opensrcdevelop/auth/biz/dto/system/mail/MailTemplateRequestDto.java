package cn.opensrcdevelop.auth.biz.dto.system.mail;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "邮件模版请求")
@Data
public class MailTemplateRequestDto {

    @Schema(description = "邮件模版ID")
    @NotBlank
    private String id;

    @Schema(description = "主题")
    @NotBlank
    private String subject;

    @Schema(description = "发送人")
    @NotBlank
    private String sender;

    @Schema(description = "模版")
    @NotBlank
    private String content;
}
