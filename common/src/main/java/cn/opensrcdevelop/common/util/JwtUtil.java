package cn.opensrcdevelop.common.util;

import cn.opensrcdevelop.common.exception.ServerException;
import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.JWSSigner;
import com.nimbusds.jose.JWSVerifier;
import com.nimbusds.jose.crypto.MACSigner;
import com.nimbusds.jose.crypto.MACVerifier;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;
import io.vavr.control.Try;
import org.apache.commons.collections4.MapUtils;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.Map;

@SuppressWarnings("unused")
public class JwtUtil {

    private JwtUtil() {}

    /**
     * 创建 Jwt
     * 签名算法：HS256 - HMAC with SHA-256, requires 256+ bit secret
     *
     * @param claims 负载
     * @param secret 密钥
     * @param liveTime 存活时间
     * @param timeUnit 时间单位
     * @return Jwt
     */
    public static String createJwtWithHS256(Map<String, Object> claims, String secret, long liveTime, ChronoUnit timeUnit) {
        return Try.of(() -> {
            JWSSigner signer = new MACSigner(secret);
            JWTClaimsSet.Builder claimSetBuilder = new JWTClaimsSet.Builder();

            LocalDateTime signTime = LocalDateTime.now();
            claimSetBuilder.issueTime(Date.from(signTime.atZone(ZoneId.systemDefault()).toInstant()));
            claimSetBuilder.expirationTime(Date.from(signTime.plus(liveTime, timeUnit).atZone(ZoneId.systemDefault()).toInstant()));

            if (MapUtils.isNotEmpty(claims)) {
                claims.forEach(claimSetBuilder::claim);
            }

            JWTClaimsSet claimsSet = claimSetBuilder.build();
            SignedJWT signedJWT = new SignedJWT(new JWSHeader(JWSAlgorithm.HS256), claimsSet);
            signedJWT.sign(signer);

            return signedJWT.serialize();
        }).getOrElseThrow(ServerException::new);
    }

    /**
     * 校验 Jwt
     * 签名算法：HS256 - HMAC with SHA-256, requires 256+ bit secret
     *
     * @param jwt jwt
     * @param secret 密钥
     * @return 校验结果
     */
    public static boolean verifyJwtWithHS256(String jwt, String secret) {
        return Try.of(() -> {
            JWSVerifier jwsVerifier = new MACVerifier(secret);
            SignedJWT signedJWT = SignedJWT.parse(jwt);
            return signedJWT.verify(jwsVerifier) && LocalDateTime.now().isBefore(signedJWT.getJWTClaimsSet().getExpirationTime().toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime());
        }).getOrElseThrow(ServerException::new);
    }

    /**
     * 获取 Jwt 负载
     *
     * @param jwt jwt
     * @return 负载
     */
    public static Map<String, Object> getJwtClaimsWithHS256(String jwt) {
        return Try.of(() -> SignedJWT.parse(jwt).getJWTClaimsSet().getClaims()).getOrElseThrow(ServerException::new);
    }
}
