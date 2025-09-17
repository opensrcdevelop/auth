package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "测试数据源连接响应")
@Builder
@Setter
@Getter
public class TestDataSourceConnResponseDto {

    @Schema(description = "是否连接成功")
    private Boolean connected;
}
