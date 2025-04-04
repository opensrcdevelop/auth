package cn.opensrcdevelop.auth.biz.dto.client;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

@Schema(description = "获取客户端详情信息响应")
@Data
public class ClientResponseDto {

    @Schema(description = "ID")
    private String id;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "描述")
    private String desc;

    @Schema(description = "回调地址")
    private String redirectUri;

    @Schema(description = "授权类型")
    private List<String> grantTypes;

    @Schema(description = "认证方式")
    private List<String> authenticationMethods;

    @Schema(description = "OIDC scope")
    private List<String> scopes;

    @Schema(description = "授权码过期时间")
    private Long authorizationCodeTimeToLive;

    @Schema(description = "访问令牌过期时间")
    private Long accessTokenTimeToLive;

    @Schema(description = "刷新令牌过期时间")
    private Long refreshTokenTimeToLive;
}
