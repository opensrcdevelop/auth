package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.entity.OidcClaimScopeMapping;
import cn.opensrcdevelop.auth.biz.mapper.OidcClaimScopeMappingMapper;
import cn.opensrcdevelop.auth.biz.service.OidcClaimScopeMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class OidcClaimScopeMappingServiceImpl extends ServiceImpl<OidcClaimScopeMappingMapper, OidcClaimScopeMapping> implements OidcClaimScopeMappingService {
}
