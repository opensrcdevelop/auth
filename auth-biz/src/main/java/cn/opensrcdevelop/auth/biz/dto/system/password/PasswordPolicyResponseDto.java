package cn.opensrcdevelop.auth.biz.dto.system.password;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.Builder;
import lombok.Data;

@Schema(description = "密码策略响应")
@Data
@Builder
public class PasswordPolicyResponseDto {

    @Schema(description = "策略ID")
    private String id;

    @Schema(description = "策略名称")
    private String name;

    @Schema(description = "策略描述")
    private String desc;

    @Schema(description = "是否启用")
    private Boolean enabled;

    @Schema(description = "密码强度")
    private Integer passwordStrength;

    @Schema(description = "最小长度")
    private Integer minLength;

    @Schema(description = "最大长度")
    private Integer maxLength;

    @Schema(description = "是否必须包含数字")
    private Boolean requireNumber;

    @Schema(description = "是否必须包含小写字母")
    private Boolean requireLowerCase;

    @Schema(description = "是否必须包含大写字母")
    private Boolean requireUpperCase;

    @Schema(description = "是否必须包含特殊字符")
    private Boolean requireSpecialChar;

    @Schema(description = "至少需要满足的字符类型数量")
    private Integer minCharTypeCount;

    @Schema(description = "是否禁止包含用户信息")
    private Boolean prohibitUserInfo;

    @Schema(description = "是否禁止全部为单一字符")
    private Boolean prohibitSingleChar;

    @Schema(description = "是否禁止全部为连续字符")
    private Boolean prohibitConsecutiveChar;

    @Schema(description = "是否禁止包含连续字符")
    private Boolean prohibitContainConsecutiveChar;

    @Schema(description = "禁止包含的连续字符的最小长度")
    private Integer minConsecutiveCharLength;

    @Schema(description = "是否禁止包含连续重复字符")
    private Boolean prohibitContainRepeatChar;

    @Schema(description = "禁止包含的连续重复字符的最小长度")
    private Integer minRepeatCharLength;

    @Schema(description = "是否禁止使用特定密码")
    private Boolean prohibitSpecificPassword;

    @Schema(description = "禁止使用的特定密码列表")
    private List<String> prohibitedPasswordList;

    @Schema(description = "是否开启用户登录密码强度检查")
    private Boolean enablePasswordDetection;

    @Schema(description = "是否开启强制修改密码")
    private Boolean enableForceChangePassword;

    @Schema(description = "强制修改密码周期")
    private Integer forcedCycle;

    @Schema(description = "强制修改密码周期单位")
    private String forcedCycleUnit;

    @Schema(description = "密码到期提醒周期")
    private Integer remindCycle;

    @Schema(description = "密码到期提醒周期单位")
    private String remindCycleUnit;

    @Schema(description = "用户ID列表")
    private List<String> userIds;

    @Schema(description = "用户组ID列表")
    private List<String> userGroupIds;
}
