package cn.opensrcdevelop.auth.biz.dto;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.List;

@Schema(description = "创建用户请求")
@Data
public class UserRequestDto {

    @Schema(description = "用户ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String userId;

    @Schema(description = "用户名")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String username;

    @Schema(description = "密码")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String password;

    @Schema(description = "手机号码")
    @NotBlankStr
    private String phoneNumber;

    @Schema(description = "邮箱地址")
    @NotBlankStr
    private String emailAddress;

    @Schema(description = "启用多因素认证")
    private Boolean enableMfa;

    @Schema(description = "禁用账号")
    private Boolean locked;

    @Schema(description = "需要变更密码")
    private Boolean needChangePwd;

    @Schema(description = "控制台访问")
    private Boolean consoleAccess;

    @Schema(description = "创建用户时发送通知邮件")
    private Boolean sendEmail;

    @Schema(description = "用户扩展属性")
    @Valid
    private List<UserAttrMappingRequestDto> attributes;
}
