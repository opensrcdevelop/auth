package cn.opensrcdevelop.auth.biz.entity.client.oidc;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.util.List;
import lombok.Data;

/**
 * OIDC scope
 */
@Data
@TableName("t_oidc_scope")
@EntityName("OIDC Scope")
public class OidcScope {

    @TableId(type = IdType.INPUT)
    @PropertyName("OIDC Scope ID")
    private String scopeId;

    @PropertyName("OIDC Scope 名称")
    private String scopeName;

    @TableField(exist = false)
    private List<OidcClaim> claims;
}
