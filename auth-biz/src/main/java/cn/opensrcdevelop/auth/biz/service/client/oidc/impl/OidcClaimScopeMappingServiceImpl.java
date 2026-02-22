package cn.opensrcdevelop.auth.biz.service.client.oidc.impl;

import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaimScopeMapping;
import cn.opensrcdevelop.auth.biz.mapper.client.oidc.OidcClaimScopeMappingMapper;
import cn.opensrcdevelop.auth.biz.service.client.oidc.OidcClaimScopeMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class OidcClaimScopeMappingServiceImpl extends ServiceImpl<OidcClaimScopeMappingMapper, OidcClaimScopeMapping>
        implements
            OidcClaimScopeMappingService {
}
