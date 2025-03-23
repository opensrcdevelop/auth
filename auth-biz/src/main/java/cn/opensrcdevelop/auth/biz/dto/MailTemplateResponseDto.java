package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Schema(description = "邮件模版响应")
@Builder
@Data
public class MailTemplateResponseDto {

    @Schema(description = "邮件模版ID")
    private String id;

    @Schema(description = "邮件模版名称")
    private String name;

    @Schema(description = "邮件模版标识")
    private String code;

    @Schema(description = "邮件模版内容")
    private String content;

    @Schema(description = "邮件模版参数")
    private List<MailTemplateParamResponseDto> parameters;

    @Schema(description = "邮件模版描述")
    private String desc;

    @Schema(description = "主题")
    private String subject;

    @Schema(description = "发件人")
    private String sender;
}
