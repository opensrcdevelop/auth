package cn.opensrcdevelop.auth.biz.entity.client.oidc;

import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttr;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

/**
 * OIDC claim
 */
@Data
@TableName("t_oidc_claim")
public class OidcClaim {

    @TableId(type = IdType.INPUT)
    private String claimId;

    private String claimName;

    private String userAttrId;

    @TableField(exist = false)
    private UserAttr userAttr;
}
