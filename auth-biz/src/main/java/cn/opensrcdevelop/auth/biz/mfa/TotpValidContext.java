package cn.opensrcdevelop.auth.biz.mfa;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Data
public class TotpValidContext implements Serializable {

    @Serial
    private static final long serialVersionUID = -4403775756555877827L;

    /** Totp 校验是否通过 */
    private Boolean valid;

    /** 用户ID */
    private String userId;
}
