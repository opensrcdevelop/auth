package cn.opensrcdevelop.auth.biz.dto.user.excel;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.HashMap;
import java.util.Map;
import lombok.Data;

@Schema(description = "用户 Excel 导入数据")
@Data
public class UserExcelImportDto {

    @Schema(description = "操作类型：0-添加，1-更新，2-删除")
    private Integer operationType;

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "邮箱地址")
    private String emailAddress;

    @Schema(description = "手机号码")
    private String phoneNumber;

    @Schema(description = "禁用状态")
    private Boolean locked;

    @Schema(description = "控制台访问权限")
    private Boolean consoleAccess;

    @Schema(description = "启用多因素认证")
    private Boolean enableMfa;

    @Schema(description = "扩展属性（动态字段）")
    private Map<String, Object> extAttrs = new HashMap<>();
}
