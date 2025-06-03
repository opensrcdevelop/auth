package cn.opensrcdevelop.auth.biz.dto.identity;

import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.Map;

@Schema(description = "请求配置请求")
@Data
public class RequestConfigRequestDto {

    @Schema(description = "请求方式")
    @NotBlank(groups = { TokenReq.class, UserInfoReq.class  })
    @EnumValue({ "GET", "POST" })
    private String method;

    @Schema(description = "query 参数")
    @NotEmpty(groups = { AuthzReq.class, GetReq.class  })
    private Map<String, Object> params;

    @Schema(description = "body 参数")
    @NotEmpty(groups = { PostReq.class  })
    private Map<String, Object> body;

    @Schema(description = "请求头")
    private Map<String, Object> headers;

    @Schema(description = "path 变量")
    private Map<String, Object> pathVariables;

    @Schema(description = "accessToken 属性名称")
    private String accessTokenAttr;

    public interface AuthzReq {}
    public interface TokenReq {}
    public interface UserInfoReq {}
    public interface GetReq {}
    public interface PostReq {}
}
