package cn.opensrcdevelop.auth.biz.dto.client;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "创建客户端 / 更新客户端密钥响应")
@Data
public class CreateOrUpdateSecretClientResponseDto {

    @Schema(description = "客户端ID")
    private String id;

    @Schema(description = "客户端密钥")
    private String secret;

    @Schema(description = "回调地址")
    private String redirectUri;
}
