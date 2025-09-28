package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "ChatBI 请求")
@Data
public class ChatBIRequestDto {

    @Schema(description = "对话ID")
    private String chatId;

    @Schema(description = "问题ID")
    @NotBlank
    private String questionId;

    @Schema(description = "模型提供商ID")
    @NotBlank
    private String modelProviderId;

    @Schema(description = "模型")
    @NotBlank
    private String model;

    @Schema(description = "数据源ID")
    @NotBlank(groups = ChatBIRequestDto.GenerateChart.class)
    private String dataSourceId;

    @Schema(description = "问题")
    @NotBlank(groups = ChatBIRequestDto.GenerateChart.class)
    private String question;

    @Schema(description = "图表ID")
    @NotBlank(groups = ChatBIRequestDto.AnalyzeData.class)
    private String chartId;

    @Schema(description = "是否生成报告")
    private boolean generateReport;

    public static interface GenerateChart {}
    public static interface AnalyzeData {}
}
