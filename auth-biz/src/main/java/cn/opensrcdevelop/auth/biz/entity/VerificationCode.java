package cn.opensrcdevelop.auth.biz.entity;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.time.Instant;

/**
 * 验证码
 */
@Data
public class VerificationCode implements Serializable {

    @Serial
    private static final long serialVersionUID = -7266905278310369706L;

    /** 验证码 */
    private String code;

    /** 过期时间 */
    private Instant expireTime;
}
