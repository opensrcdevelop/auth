package cn.opensrcdevelop.auth.biz.service.identity;

import cn.opensrcdevelop.auth.biz.dto.identity.UserBindingResponseDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.entity.identity.ThirdAccount;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;
import java.util.Map;

public interface ThirdAccountService extends IService<ThirdAccount> {

    User bind(List<Map<String, Object>> attributesList, IdentitySourceRegistration identitySourceRegistration);

    User bind(String userId, List<Map<String, Object>> attributesList, IdentitySourceRegistration identitySourceRegistration);

    PageData<UserBindingResponseDto> getUserBindingList(String id, int page, int size, String keyword);
}
