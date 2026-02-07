/**
 * 变更：
 *      1. 创建 WebAuthn 凭证表 t_webauthn_credential
 *      2. 重命名字段：mfa_secret -> totp_secret, mfa_device_bind -> totp_device_bind
 *
 */

-- ----------------------------
-- 1. 创建 WebAuthn 凭证表
-- ----------------------------
CREATE TABLE IF NOT EXISTS t_webauthn_credential (
    id BIGSERIAL PRIMARY KEY,
    credential_id VARCHAR(255) NOT NULL UNIQUE,
    user_id VARCHAR(50) NOT NULL,
    public_key BYTEA NOT NULL,
    counter BIGINT NOT NULL DEFAULT 0,
    transports TEXT,
    device_type VARCHAR(100),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_used_at TIMESTAMP,
    deleted BOOLEAN DEFAULT FALSE,
    create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    create_by VARCHAR(50),
    update_time TIMESTAMP,
    update_by VARCHAR(50),
    version INTEGER DEFAULT 1
);

-- 创建用户ID索引
CREATE INDEX IF NOT EXISTS idx_webauthn_credential_user_id ON t_webauthn_credential(user_id);

-- 创建凭证ID索引（用于认证时快速查找）
CREATE INDEX IF NOT EXISTS idx_webauthn_credential_credential_id ON t_webauthn_credential(credential_id) WHERE deleted = FALSE;

-- ----------------------------
-- 2. 重命名字段（master 库已在 V1.0.22 中执行）
-- ----------------------------
-- ALTER TABLE IF EXISTS t_user RENAME COLUMN mfa_secret TO totp_secret;
-- ALTER TABLE IF EXISTS t_user RENAME COLUMN mfa_device_bind TO totp_device_bind;

-- ----------------------------
-- 3. 添加缺失的 BaseEntity 字段
-- ----------------------------
ALTER TABLE t_webauthn_credential ADD COLUMN IF NOT EXISTS create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE t_webauthn_credential ADD COLUMN IF NOT EXISTS create_by VARCHAR(50);
ALTER TABLE t_webauthn_credential ADD COLUMN IF NOT EXISTS update_time TIMESTAMP;
ALTER TABLE t_webauthn_credential ADD COLUMN IF NOT EXISTS update_by VARCHAR(50);
ALTER TABLE t_webauthn_credential ADD COLUMN IF NOT EXISTS version INTEGER DEFAULT 1;

-- ----------------------------
-- COMMENT
-- ----------------------------
COMMENT ON COLUMN t_webauthn_credential.credential_id IS '凭证ID（Base64URL编码）';
COMMENT ON COLUMN t_webauthn_credential.user_id IS '关联用户ID';
COMMENT ON COLUMN t_webauthn_credential.public_key IS '公钥材料（COSE格式）';
COMMENT ON COLUMN t_webauthn_credential.counter IS '签名计数器，用于防止重放攻击';
COMMENT ON COLUMN t_webauthn_credential.transports IS '传输类型（usb, nfc, hybrid, internal），逗号分隔';
COMMENT ON COLUMN t_webauthn_credential.device_type IS '设备类型（platform、cross-platform）';
COMMENT ON COLUMN t_webauthn_credential.last_used_at IS '最后使用时间';
COMMENT ON COLUMN t_webauthn_credential.deleted IS '软删除标记';
COMMENT ON COLUMN t_user.totp_secret IS 'TOTP 密钥';
COMMENT ON COLUMN t_user.totp_device_bind IS 'TOTP 设备绑定状态';
COMMENT ON COLUMN t_webauthn_credential.create_time IS '创建时间';
COMMENT ON COLUMN t_webauthn_credential.create_by IS '创建人';
COMMENT ON COLUMN t_webauthn_credential.update_time IS '更新时间';
COMMENT ON COLUMN t_webauthn_credential.update_by IS '更新人';
COMMENT ON COLUMN t_webauthn_credential.version IS '版本号';
