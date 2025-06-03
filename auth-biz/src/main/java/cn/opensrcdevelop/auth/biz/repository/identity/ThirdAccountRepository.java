package cn.opensrcdevelop.auth.biz.repository.identity;

import cn.opensrcdevelop.auth.biz.entity.identity.ThirdAccount;
import com.baomidou.mybatisplus.core.metadata.IPage;

public interface ThirdAccountRepository {

    void searchUserBindings(IPage<ThirdAccount> page, String registrationId, String keyword);
}
