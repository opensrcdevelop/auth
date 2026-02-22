package cn.opensrcdevelop.auth.biz.repository.auth;

import cn.opensrcdevelop.auth.biz.entity.auth.WebAuthnCredential;
import cn.opensrcdevelop.auth.biz.mapper.auth.WebAuthnCredentialMapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

/**
 * WebAuthn 凭证 Repository 实现
 */
@Repository
@RequiredArgsConstructor
public class WebAuthnCredentialRepositoryImpl implements WebAuthnCredentialRepository {

    private final WebAuthnCredentialMapper webAuthnCredentialMapper;

    /**
     * 根据凭证ID查找凭证
     *
     * @param credentialId
     *            凭证ID
     * @return 凭证信息
     */
    @Override
    public WebAuthnCredential findByCredentialId(String credentialId) {
        return webAuthnCredentialMapper.selectOne(Wrappers.<WebAuthnCredential>lambdaQuery()
                .eq(WebAuthnCredential::getCredentialId, credentialId));
    }

    /**
     * 根据用户ID查找所有未删除的凭证
     *
     * @param userId
     *            用户ID
     * @return 凭证列表
     */
    @Override
    public List<WebAuthnCredential> findAllByUserId(String userId) {
        return webAuthnCredentialMapper.selectList(Wrappers.<WebAuthnCredential>lambdaQuery()
                .eq(WebAuthnCredential::getUserId, userId)
                .orderByDesc(WebAuthnCredential::getCreateTime));
    }

    /**
     * 保存凭证
     *
     * @param credential
     *            凭证信息
     */
    @Override
    public void save(WebAuthnCredential credential) {
        if (credential.getDeleted() == null) {
            credential.setDeleted(false);
        }
        webAuthnCredentialMapper.insert(credential);
    }

    /**
     * 更新凭证
     *
     * @param credential
     *            凭证信息
     */
    @Override
    public void update(WebAuthnCredential credential) {
        webAuthnCredentialMapper.updateById(credential);
    }

    /**
     * 删除凭证（软删除）
     *
     * @param credentialId
     *            凭证ID
     */
    @Override
    public void deleteByCredentialId(String credentialId) {
        webAuthnCredentialMapper.delete(
                Wrappers.<WebAuthnCredential>lambdaQuery().eq(WebAuthnCredential::getCredentialId, credentialId));
    }

    /**
     * 根据用户ID统计凭证数量
     *
     * @param userId
     *            用户ID
     * @return 凭证数量
     */
    @Override
    public long countByUserId(String userId) {
        return webAuthnCredentialMapper.selectCount(Wrappers
                .<WebAuthnCredential>lambdaQuery().eq(WebAuthnCredential::getUserId, userId));
    }

    /**
     * 清除用户所有 Passkey 凭证
     *
     * @param userId
     *            用户ID
     */
    @Override
    public void deleteByUserId(String userId) {
        webAuthnCredentialMapper
                .delete(Wrappers.<WebAuthnCredential>lambdaQuery().eq(WebAuthnCredential::getUserId, userId));
    }
}
