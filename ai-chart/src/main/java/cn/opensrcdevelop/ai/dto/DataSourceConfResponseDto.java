package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

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

    @Schema(description = "数据源描述")
    private String desc;

    @Schema(description = "是否启用")
    private Boolean enabled;

    @Schema(description = "数据库")
    private String database;

    @Schema(description = "模式")
    private String schema;

    @Schema(description = "主机地址")
    private String host;

    @Schema(description = "端口号")
    private Integer port;

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "密码")
    private String password;

    @Schema(description = "jdbc参数")
    private String jdbcParams;

    @Schema(description = "表数量")
    private Long tableCount;

    @Schema(description = "最后同步表时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime lastSyncTableTime;

    @Schema(description = "同步表次数")
    private Long syncTableCount;
}
