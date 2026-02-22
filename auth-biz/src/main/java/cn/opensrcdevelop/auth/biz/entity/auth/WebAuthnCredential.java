package cn.opensrcdevelop.auth.biz.entity.auth;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * WebAuthn 凭证实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_webauthn_credential")
@EntityName("WebAuthn凭证")
public class WebAuthnCredential extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -5859507849014293865L;

    @TableId(type = IdType.AUTO)
    private Long id;

    /**
     * 凭证ID（Base64URL编码）
     */
    @PropertyName("凭证ID")
    private String credentialId;

    /**
     * 关联用户ID
     */
    @PropertyName("关联用户ID")
    private String userId;

    /**
     * 公钥（JSON格式）
     */
    @PropertyName("公钥")
    private String publicKey;

    /**
     * 签名计数器，用于防止重放攻击
     */
    @PropertyName("签名计数器")
    private Long counter;

    /**
     * 传输类型（usb, nfc, hybrid, internal），逗号分隔
     */
    @PropertyName("传输类型")
    private String transports;

    /**
     * 设备类型（platform、cross-platform）
     */
    @PropertyName("设备类型")
    private String deviceType;

    /**
     * 最后使用时间
     */
    @PropertyName("最后使用时间")
    private LocalDateTime lastUsedAt;
}
