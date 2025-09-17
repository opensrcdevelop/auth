package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.ai.enums.DataSourceType;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "测试数据源连接请求")
@Data
public class TestDataSourceConnRequestDto {

    @Schema(description = "数据源类型")
    @NotNull
    private DataSourceType type;

    @Schema(description = "主机地址")
    @NotBlank
    private String host;

    @Schema(description = "端口")
    @NotNull
    @Min(1)
    private Integer port;

    @Schema(description = "数据库")
    @NotBlank
    private String database;

    @Schema(description = "用户名")
    @NotBlank
    private String username;

    @Schema(description = "密码")
    @NotBlank
    private String password;
}
