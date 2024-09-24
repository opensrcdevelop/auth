package cn.opensrcdevelop.auth.biz.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.*;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;

/**
 * 用户实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_user")
@JsonIgnoreProperties(ignoreUnknown = true)
public class User extends BaseEntity implements UserDetails, Serializable {

    @Serial
    private static final long serialVersionUID = -37127788129570340L;

    @TableId(type = IdType.INPUT)
    private String userId;

    private String username;

    @JsonIgnore
    private String password;

    private String phoneNumber;

    private String emailAddress;

    private Boolean enableMfa;

    private String mfaSecret;

    private Boolean needChangePwd;

    private Boolean mfaDeviceBind;

    private Boolean locked;

    private Boolean consoleAccess;

    private LocalDateTime lastLoginTime;

    private String lastLoginIp;

    private String lastLoginDeviceType;

    private String lastLoginDeviceOs;

    private Integer loginFailedCnt;

    @TableField(exist = false)
    private Collection<GrantedAuthority> authorities;

    @TableField(exist = false)
    private List<Role> roles;

    @TableField(exist = false)
    private List<UserGroup> userGroups;

    @TableField(exist = false)
    private List<Permission> permissions;

    @TableField(exist = false)
    private List<UserAttr> userAttrs;

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return this.authorities;
    }

    @Override
    public String getPassword() {
        return this.password;
    }

    @Override
    public String getUsername() {
        return this.username;
    }

    @Override
    public boolean isEnabled() {
        return BooleanUtils.isNotTrue(locked);
    }
}
