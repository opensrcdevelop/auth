package cn.opensrcdevelop.auth.biz.mapper.auth;

import cn.opensrcdevelop.auth.biz.entity.auth.WebAuthnCredential;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * WebAuthn 凭证 Mapper
 */
@Mapper
public interface WebAuthnCredentialMapper extends BaseMapper<WebAuthnCredential> {

}
