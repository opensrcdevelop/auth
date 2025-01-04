package cn.opensrcdevelop.auth.support;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.nimbusds.jose.KeySourceException;
import com.nimbusds.jose.jwk.JWK;
import com.nimbusds.jose.jwk.JWKSelector;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.RSAKey;
import com.nimbusds.jose.jwk.source.JWKSource;
import com.nimbusds.jose.proc.SecurityContext;
import io.vavr.control.Try;
import org.apache.commons.lang3.StringUtils;

import java.security.KeyPair;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

public class DelegatingJWKSource implements JWKSource<SecurityContext> {

    private final ReentrantLock lock = new ReentrantLock();

    @Override
    public List<JWK> get(JWKSelector jwkSelector, SecurityContext context) throws KeySourceException {
        return jwkSelector.select(getJwkSet());
    }

    private JWKSet getJwkSet() {
        lock.lock();
        try {
            String tenantCode = TenantContextHolder.getTenantContext().getTenantCode();
            String jwkSourceStr = RedisUtil.get(AuthConstants.JWK_REDIS_KEY + tenantCode, String.class);
            JWKSet jwkSet;
            if (StringUtils.isNotEmpty(jwkSourceStr)) {
                jwkSet = Try.of(() -> JWKSet.parse(jwkSourceStr)).getOrElseThrow(ServerException::new);
            } else {
                KeyPair keyPair = CommonUtil.generateRsaKey();
                RSAPublicKey publicKey = (RSAPublicKey) keyPair.getPublic();
                RSAPrivateKey privateKey = (RSAPrivateKey) keyPair.getPrivate();
                RSAKey rsaKey = new RSAKey.Builder(publicKey)
                        .privateKey(privateKey)
                        .keyID(CommonUtil.getBase64StringKey(32))
                        .build();
                jwkSet = new JWKSet(rsaKey);
                RedisUtil.set(AuthConstants.JWK_REDIS_KEY + tenantCode, jwkSet.toString(false));
            }
            return jwkSet;
        } finally {
            lock.unlock();
        }
    }
}
