package cn.opensrcdevelop.auth.biz.mfa;

import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.tenant.support.TenantContext;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import org.apache.commons.codec.binary.Base32;

public class TotpAuthenticator {

    private TotpAuthenticator() {
    }

    private static final String RANDOM_NUMBER_ALGORITHM = "SHA1PRNG";
    private static final String ISSUER = SpringContextUtil.getProperty("spring.application.name", "auth-server");
    private static final String TENANT_ISSUER_FORMAT = "%s - %s";
    private static final int SECRET_SIZE = 10;
    // 偏移时间 1 ~ 17
    private static final int WINDOW_SIZE = 0;
    // 时间间隔（秒）
    private static final long TIME_PERIOD = 30L;
    private static final String TOTP_QRCODE_FORMAT = "otpauth://totp/%s?secret=%s&issuer=%s";
    private static final Base32 BASE32 = new Base32();

    /**
     * 获取 TOTP 密钥
     *
     * @return 密钥
     */
    public static String generateSecretKey() {
        try {
            SecureRandom sr = SecureRandom.getInstance(RANDOM_NUMBER_ALGORITHM);
            sr.setSeed(ISSUER.getBytes(StandardCharsets.UTF_8));
            byte[] randomBytes = sr.generateSeed(SECRET_SIZE);
            return BASE32.encodeAsString(randomBytes).replaceAll("=+$", "");
        } catch (NoSuchAlgorithmException e) {
            throw new ServerException(e);
        }
    }

    /**
     * 获取二维码字符串
     *
     * @param username
     *            用户名
     * @param secret
     *            密钥
     * @return 二维码字符串
     */
    public static String getQrCodeString(String username, String secret) {
        TenantContext tenantContext = TenantContextHolder.getTenantContext();
        if (tenantContext.isDefaultTenant()) {
            return String.format(TOTP_QRCODE_FORMAT, username, secret, ISSUER);
        } else {
            return String.format(TOTP_QRCODE_FORMAT, username, secret,
                    String.format(TENANT_ISSUER_FORMAT, ISSUER, tenantContext.getTenantName()));
        }
    }

    /**
     * 验证一次性密码是否正确
     *
     * @param secret
     *            密钥
     * @param code
     *            用户提交的密码
     * @param time
     *            当前时间
     * @return 验证结果
     */
    public static boolean checkCode(String secret, long code, long time) {
        byte[] decodedSecret = BASE32.decode(secret);
        long t = (time / 1000L) / TIME_PERIOD;
        for (int i = -WINDOW_SIZE; i <= WINDOW_SIZE; i++) {
            long hash;
            try {
                hash = verifyCode(decodedSecret, t + i);
            } catch (Exception e) {
                throw new ServerException(e);
            }
            if (hash == code) {
                return true;
            }
        }
        return false;
    }

    private static int verifyCode(byte[] key, long t) throws NoSuchAlgorithmException, InvalidKeyException {
        byte[] data = new byte[8];
        long value = t;
        for (int i = 8; i-- > 0; value >>>= 8) {
            data[i] = (byte) value;
        }
        SecretKeySpec signKey = new SecretKeySpec(key, "HmacSHA1");
        Mac mac = Mac.getInstance("HmacSHA1");
        mac.init(signKey);
        byte[] hash = mac.doFinal(data);
        int offset = hash[20 - 1] & 0xF;
        // We're using a long because Java hasn't got unsigned int.
        long truncatedHash = 0;
        for (int i = 0; i < 4; ++i) {
            truncatedHash <<= 8;
            // We are dealing with signed bytes:
            // we just keep the first byte.
            truncatedHash |= (hash[offset + i] & 0xFF);
        }
        truncatedHash &= 0x7FFFFFFF;
        truncatedHash %= 1000000;
        return (int) truncatedHash;
    }
}
