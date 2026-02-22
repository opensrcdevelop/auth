package cn.opensrcdevelop.auth.support;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.nimbusds.jose.KeySourceException;
import com.nimbusds.jose.jwk.JWK;
import com.nimbusds.jose.jwk.JWKSelector;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.source.JWKSource;
import com.nimbusds.jose.proc.SecurityContext;
import io.vavr.control.Try;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.redisson.api.RLock;

@RequiredArgsConstructor
public class DelegatingJWKSource implements JWKSource<SecurityContext> {

    private final SystemSettingService systemSettingService;

    @Override
    public List<JWK> get(JWKSelector jwkSelector, SecurityContext context) throws KeySourceException {
        RLock lock = RedisUtil
                .getLock(SystemSettingService.LOCK_KEY_JWK + TenantContextHolder.getTenantContext().getTenantCode());
        try {
            lock.lock();
            // 1. 从缓存中获取
            String cacheKey = AuthConstants.JWK_REDIS_KEY + TenantContextHolder.getTenantContext().getTenantCode();
            if (!RedisUtil.hasKey(cacheKey)) {
                // 2. 缓存中不存在，轮换密钥
                systemSettingService.rotateJwtSecret();
            }
            String jwkSourceStr = RedisUtil.get(cacheKey, String.class);
            return jwkSelector.select(Try.of(() -> JWKSet.parse(jwkSourceStr)).getOrElseThrow(ServerException::new));
        } finally {
            lock.unlock();
        }
    }
}
