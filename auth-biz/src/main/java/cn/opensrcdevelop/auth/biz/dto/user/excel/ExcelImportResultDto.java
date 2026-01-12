package cn.opensrcdevelop.auth.biz.dto.user.excel;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Schema(description = "Excel 导入结果")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ExcelImportResultDto {

    @Schema(description = "成功数量")
    private Integer successCount;

    @Schema(description = "失败数量")
    private Integer failureCount;

    @Schema(description = "错误详情列表")
    private List<ExcelImportErrorDto> errors;
}
