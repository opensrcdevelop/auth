package cn.opensrcdevelop.auth.biz.mapper.client.oidc;

import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaimScopeMapping;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface OidcClaimScopeMappingMapper extends BaseMapper<OidcClaimScopeMapping> {
}
