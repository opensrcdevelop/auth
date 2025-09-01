package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "数据源配置响应")
@Builder
@Getter
@Setter
public class DataSourceConfResponseDto {

    @Schema(description = "数据源ID")
    private String id;

    @Schema(description = "数据源名称")
    private String name;

    @Schema(description = "数据源类型")
    private String type;

    @Schema(description = "数据库")
    private String database;

    @Schema(description = "主机地址")
    private String host;

    @Schema(description = "端口")
    private String port;

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "密码")
    private String password;
}
