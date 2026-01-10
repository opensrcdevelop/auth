package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "数据源配置请求")
@Data
public class DataSourceConfRequestDto {

    @Schema(description = "数据源ID")
    @NotBlank(groups = {ValidationGroups.Operation.UPDATE.class})
    private String id;

    @Schema(description = "数据源名称")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String name;

    @Schema(description = "数据源类型")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    private String type;

    @Schema(description = "数据源描述")
    private String desc;

    @Schema(description = "是否启用")
    private Boolean enabled;

    @Schema(description = "数据库")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    private String database;

    @Schema(description = "模式")
    private String schema;

    @Schema(description = "主机地址")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String host;

    @Schema(description = "端口号")
    @NotNull(groups = {ValidationGroups.Operation.INSERT.class})
    @Min(value = 1, groups = {ValidationGroups.Operation.INSERT.class})
    private Integer port;

    @Schema(description = "用户名")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String username;

    @Schema(description = "密码")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String password;

    @Schema(description = "jdbc参数")
    private String jdbcParams;
}
