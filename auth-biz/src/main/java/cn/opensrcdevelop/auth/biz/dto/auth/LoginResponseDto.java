package cn.opensrcdevelop.auth.biz.dto.auth;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.Data;

@Schema(description = "登录结果响应")
@Data
public class LoginResponseDto {

    @Schema(description = "是否需要变更密码")
    private Boolean needChangePwd;

    @Schema(description = "变更密码类型")
    private Integer changePwdType;

    @Schema(description = "是否启用多因素认证")
    private Boolean enableMfa;

    @Schema(description = "MFA方式列表：TOTP（时间一次性密码）、WEBAUTHN（WebAuthn/Passkey）")
    private List<String> mfaMethods;

    @Schema(description = "TOTP 设备绑定状态")
    private Boolean totpDeviceBind;

    @Schema(description = "TOTP 设备绑定二维码数据")
    private String bindTotpDeviceQrCode;

    @Schema(description = "控制台访问")
    private Boolean consoleAccess;

    @Schema(description = "是否有 Passkey")
    private Boolean hasPasskey;

    @Schema(description = "WebAuthn/Passkey 注册选项")
    private WebAuthnRegisterOptionsResponseDto webAuthnRegisterOptions;
}
