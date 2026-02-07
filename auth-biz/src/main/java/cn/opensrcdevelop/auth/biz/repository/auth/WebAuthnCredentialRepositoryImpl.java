package cn.opensrcdevelop.auth.biz.repository.auth;

import cn.opensrcdevelop.auth.biz.entity.auth.WebAuthnCredential;
import cn.opensrcdevelop.auth.biz.mapper.auth.WebAuthnCredentialMapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
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
        QueryWrapper<WebAuthnCredential> wrapper = new QueryWrapper<>();
        wrapper.eq("credential_id", credentialId)
                .eq("deleted", false)
                .last("LIMIT 1");
        return webAuthnCredentialMapper.selectOne(wrapper);
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
        QueryWrapper<WebAuthnCredential> wrapper = new QueryWrapper<>();
        wrapper.eq("user_id", userId)
                .eq("deleted", false)
                .orderByDesc("created_at");
        return webAuthnCredentialMapper.selectList(wrapper);
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
        UpdateWrapper<WebAuthnCredential> wrapper = new UpdateWrapper<>();
        wrapper.eq("credential_id", credentialId)
                .set("deleted", true);
        webAuthnCredentialMapper.update(null, wrapper);
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
        QueryWrapper<WebAuthnCredential> wrapper = new QueryWrapper<>();
        wrapper.eq("user_id", userId)
                .eq("deleted", false);
        return webAuthnCredentialMapper.selectCount(wrapper);
    }
}
