package cn.opensrcdevelop.auth.biz.entity.user;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.biz.entity.permission.Permission;
import cn.opensrcdevelop.auth.biz.entity.role.Role;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttr;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroup;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.oauth2.core.user.OAuth2User;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * 用户实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_user")
@EntityName("用户")
@JsonIgnoreProperties(ignoreUnknown = true)
public class User extends BaseEntity implements UserDetails, OAuth2User, Serializable {

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

    private Integer loginFailedCnt;

    private LocalDateTime lastUpdatePasswordTime;

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
    @JsonIgnore
    public Map<String, Object> getAttributes() {
        return Collections.emptyMap();
    }

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

    @Override
    public String getName() {
        return this.username;
    }
}
