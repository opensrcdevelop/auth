package cn.opensrcdevelop.auth.biz.dto.user.excel;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Schema(description = "Excel 导入错误详情")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ExcelImportErrorDto {

    @Schema(description = "行号")
    private Integer row;

    @Schema(description = "列名")
    private String column;

    @Schema(description = "错误值")
    private String errorValue;

    @Schema(description = "错误信息")
    private String message;
}
