package cn.opensrcdevelop.auth.biz.dto.auth;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

@Schema(description = "登录结果响应")
@Data
public class LoginResponseDto {

    @Schema(description = "是否需要变更密码")
    private Boolean needChangePwd;

    @Schema(description = "变更密码类型")
    private Integer changePwdType;

    @Schema(description = "是否启用多因素认证")
    private Boolean enableMfa;

    @Schema(description = "支持的MFA方式列表：TOTP（时间一次性密码）、WEBAUTHN（WebAuthn/Passkey）")
    private List<String> supportedMfaMethods;

    @Schema(description = "TOTP 设备绑定状态")
    private Boolean bound;

    @Schema(description = "二维码数据")
    private String qrCode;

    @Schema(description = "控制台访问")
    private Boolean consoleAccess;

    @Schema(description = "已绑定的 Passkey 列表")
    private List<String> credentialIds;
}
