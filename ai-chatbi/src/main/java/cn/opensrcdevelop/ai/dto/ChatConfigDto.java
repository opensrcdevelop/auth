package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.Data;

@EntityName("ChatBI 对话配置")
@Schema(description = "ChatBI 对话配置")
@Data
public class ChatConfigDto {

    @PropertyName("最大思考步数")
    @Schema(description = "最大思考步数")
    @Min(30)
    private Integer maxThinkSteps;

    @PropertyName("回答语言")
    @Schema(description = "回答语言")
    @NotBlankStr
    private String answerLanguage;

    @PropertyName("最大 LLM API 重试次数")
    @Schema(description = "最大 LLM API 重试次数")
    @Min(3)
    private Integer llmApiRetryCount;

    @PropertyName("温度")
    @Schema(description = "温度")
    @Min(0)
    @Max(1)
    private Double temperature;

    @PropertyName("最大令牌数")
    @Schema(description = "最大令牌数")
    @Min(1)
    private Long maxTokens;

    @PropertyName("最大连续工具调用次数")
    @Schema(description = "最大连续工具调用次数，触发思考提示")
    @Min(2)
    private Integer maxConsecutiveToolCalls;

    @PropertyName("SQL 结果条数限制")
    @Schema(description = "SQL 查询结果条数限制")
    @Min(1)
    private Integer sqlResultLimit;

    @PropertyName("最大 SQL 执行重试次数")
    @Schema(description = "最大 SQL 执行重试次数")
    @Min(1)
    private Integer maxSqlExecutionRetryCount;

    @PropertyName("最大 Python 执行重试次数")
    @Schema(description = "最大 Python 代码执行重试次数")
    @Min(1)
    private Integer maxPythonExecutionRetryCount;
}
