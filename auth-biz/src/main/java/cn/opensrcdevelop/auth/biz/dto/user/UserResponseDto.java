package cn.opensrcdevelop.auth.biz.dto.user;

import cn.opensrcdevelop.auth.biz.dto.role.RoleResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.group.UserGroupResponseDto;
import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Data;

@Schema(description = "用户响应")
@Data
public class UserResponseDto {

    @Schema(description = "用户ID")
    private String id;

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "手机号码")
    private String phoneNumber;

    @Schema(description = "邮箱地址")
    private String emailAddress;

    @Schema(description = "是否启用多因素认证")
    private Boolean enableMfa;

    @Schema(description = "创建时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime createTime;

    @Schema(description = "禁用状态")
    private Boolean locked;

    @Schema(description = "控制台访问")
    private Boolean consoleAccess;

    @Schema(description = "最后登录时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime lastLoginTime;

    @Schema(description = "最后登录 IP")
    private String lastLoginIp;

    @Schema(description = "最后登录设备类型")
    private String lastLoginDeviceType;

    @Schema(description = "最后登录设备操作系统")
    private String lastLoginDeviceOs;

    @Schema(description = "用户扩展属性")
    private List<UserAttrResponseDto> attributes;

    @Schema(description = "用户角色")
    private List<RoleResponseDto> roles;

    @Schema(description = "用户组")
    private List<UserGroupResponseDto> userGroups;
}
