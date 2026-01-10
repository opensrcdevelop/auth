package cn.opensrcdevelop.auth.biz.repository.identity.impl;

import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.mapper.identity.IdentitySourceRegistrationMapper;
import cn.opensrcdevelop.auth.biz.repository.identity.IdentitySourceRegistrationRepository;
import com.baomidou.mybatisplus.core.metadata.IPage;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class IdentitySourceRegistrationRepositoryImpl implements IdentitySourceRegistrationRepository {

    private final IdentitySourceRegistrationMapper identitySourceRegistrationMapper;

    @Override
    public IdentitySourceRegistration getRegistrationByCode(String code) {
        return identitySourceRegistrationMapper.getRegistrationByCode(code);
    }

    @Override
    public List<IdentitySourceRegistration> getEnabledRegistrations() {
        return identitySourceRegistrationMapper.getEnabledRegistrations();
    }

    @Override
    public void searchRegistrations(IPage<IdentitySourceRegistration> page, String keyword) {
        identitySourceRegistrationMapper.searchRegistrations(page, keyword);
    }
}
