package cn.opensrcdevelop.auth.biz.repository.auth;

import cn.opensrcdevelop.auth.biz.entity.auth.WebAuthnCredential;
import java.util.List;

/**
 * WebAuthn 凭证 Repository 接口
 */
public interface WebAuthnCredentialRepository {

    /**
     * 根据凭证ID查找凭证
     *
     * @param credentialId
     *            凭证ID
     * @return 凭证信息
     */
    WebAuthnCredential findByCredentialId(String credentialId);

    /**
     * 根据用户ID查找所有未删除的凭证
     *
     * @param userId
     *            用户ID
     * @return 凭证列表
     */
    List<WebAuthnCredential> findAllByUserId(String userId);

    /**
     * 保存凭证
     *
     * @param credential
     *            凭证信息
     */
    void save(WebAuthnCredential credential);

    /**
     * 更新凭证
     *
     * @param credential
     *            凭证信息
     */
    void update(WebAuthnCredential credential);

    /**
     * 删除凭证（软删除）
     *
     * @param credentialId
     *            凭证ID
     */
    void deleteByCredentialId(String credentialId);

    /**
     * 根据用户ID统计凭证数量
     *
     * @param userId
     *            用户ID
     * @return 凭证数量
     */
    long countByUserId(String userId);

    /**
     * 清除用户所有 Passkey 凭证
     *
     * @param userId
     *            用户ID
     */
    void deleteByUserId(String userId);
}
