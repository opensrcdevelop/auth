package cn.opensrcdevelop.auth.biz.entity.client.oidc;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

/**
 * OIDC claim scope 映射
 */
@Data
@TableName("t_oidc_claim_scope_mapping")
public class OidcClaimScopeMapping {

    private String claimId;

    private String scopeId;
}
