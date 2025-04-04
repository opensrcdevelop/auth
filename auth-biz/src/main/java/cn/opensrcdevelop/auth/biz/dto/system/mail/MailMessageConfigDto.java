package cn.opensrcdevelop.auth.biz.dto.system.mail;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import lombok.Data;

@Schema(description = "邮件消息配置 请求/响应")
@Data
public class MailMessageConfigDto {

    @Schema(description = "验证码有效期（单位：分钟）")
    @Min(1)
    private Integer codeLive;
}
