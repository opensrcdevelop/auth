package cn.opensrcdevelop.auth.biz.dto.user.excel;

import com.alibaba.excel.annotation.ExcelIgnore;
import com.alibaba.excel.annotation.ExcelProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.HashMap;
import java.util.Map;
import lombok.Data;

@Schema(description = "用户 Excel 导入数据")
@Data
public class UserExcelImportDto {

    @ExcelProperty("操作类型")
    @Schema(description = "操作类型：0-添加，1-更新，2-删除")
    private Integer operationType;

    @ExcelProperty("用户名")
    @Schema(description = "用户名")
    private String username;

    @ExcelProperty("邮箱")
    @Schema(description = "邮箱地址")
    private String emailAddress;

    @ExcelProperty("手机号")
    @Schema(description = "手机号码")
    private String phoneNumber;

    @ExcelProperty("禁用")
    @Schema(description = "禁用状态")
    private Boolean locked;

    @ExcelProperty("控制台访问")
    @Schema(description = "控制台访问权限")
    private Boolean consoleAccess;

    @ExcelProperty("启用MFA")
    @Schema(description = "启用多因素认证")
    private Boolean enableMfa;

    @ExcelIgnore
    @Schema(description = "扩展属性（动态字段）")
    private Map<String, Object> extAttrs = new HashMap<>();
}
