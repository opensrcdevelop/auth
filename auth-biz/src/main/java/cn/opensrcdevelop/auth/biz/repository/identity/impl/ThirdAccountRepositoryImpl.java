package cn.opensrcdevelop.auth.biz.repository.identity.impl;

import cn.opensrcdevelop.auth.biz.entity.identity.ThirdAccount;
import cn.opensrcdevelop.auth.biz.mapper.identity.ThirdAccountMapper;
import cn.opensrcdevelop.auth.biz.repository.identity.ThirdAccountRepository;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class ThirdAccountRepositoryImpl implements ThirdAccountRepository {

    private final ThirdAccountMapper thirdAccountMapper;

    /**
     * 检索用户绑定
     *
     * @param page 分页对象
     * @param registrationId 注册身份源ID
     * @param keyword 用户名检索关键字
     */
    @Override
    public void searchUserBindings(IPage<ThirdAccount> page, String registrationId, String keyword) {
        thirdAccountMapper.searchUserBindings(page, registrationId, keyword);
    }
}
