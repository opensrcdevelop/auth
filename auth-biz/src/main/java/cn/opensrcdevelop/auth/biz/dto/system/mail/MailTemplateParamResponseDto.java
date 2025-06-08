package cn.opensrcdevelop.auth.biz.dto.system.mail;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Data;

@Schema(description = "邮件模版参数响应")
@Data
@Builder
public class MailTemplateParamResponseDto {

    @Schema(description = "参数键")
    private String key;

    @Schema(description = "参数值")
    private String value;
}
