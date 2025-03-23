package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.AuthorizeConditionRequestDto;
import cn.opensrcdevelop.auth.biz.dto.AuthorizeRequestDto;
import cn.opensrcdevelop.auth.biz.entity.AuthorizeRecord;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface AuthorizeService extends IService<AuthorizeRecord> {

    void authorize(AuthorizeRequestDto requestDto);

    void removeAuthorization(String principalId);

    void removeAuthorization(String permissionId, String principalId);

    void removeAuthorization(List<String> permissionIds);

    void createAuthorizeCondition(AuthorizeConditionRequestDto requestDto);

    void removeAuthorizeCondition(AuthorizeConditionRequestDto requestDto);

    void updateAuthorizePriority(String authorizeId, Integer priority);
}
