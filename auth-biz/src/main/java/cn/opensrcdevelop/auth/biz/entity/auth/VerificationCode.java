package cn.opensrcdevelop.auth.biz.entity.auth;

import java.io.Serial;
import java.io.Serializable;
import java.time.Instant;
import lombok.Data;

/**
 * 验证码
 */
@Data
public class VerificationCode implements Serializable {

    @Serial
    private static final long serialVersionUID = -7266905278310369706L;

    /** 接收方 */
    private String receiver;

    /** 验证码 */
    private String code;

    /** 过期时间 */
    private Instant expireTime;
}
