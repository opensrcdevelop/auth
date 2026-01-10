package cn.opensrcdevelop.auth.biz.repository.identity;

import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import com.baomidou.mybatisplus.core.metadata.IPage;
import java.util.List;

public interface IdentitySourceRegistrationRepository {

    IdentitySourceRegistration getRegistrationByCode(String code);

    List<IdentitySourceRegistration> getEnabledRegistrations();

    void searchRegistrations(IPage<IdentitySourceRegistration> page, String keyword);
}
