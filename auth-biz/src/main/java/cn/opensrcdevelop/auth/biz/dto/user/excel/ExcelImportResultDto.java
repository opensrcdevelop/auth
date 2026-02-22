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

    @Schema(description = "创建的用户数量")
    private Integer createdCount;

    @Schema(description = "更新的用户数量")
    private Integer updatedCount;

    @Schema(description = "删除的用户数量")
    private Integer deletedCount;

    @Schema(description = "错误详情列表")
    private List<ExcelImportErrorDto> errors;
}
