package cn.opensrcdevelop.auth.biz.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.List;

/**
 * OIDC scope
 */
@Data
@TableName("t_oidc_scope")
public class OidcScope {

    @TableId(type = IdType.INPUT)
    private String scopeId;

    private String scopeName;

    @TableField(exist = false)
    private List<OidcClaim> claims;
}
