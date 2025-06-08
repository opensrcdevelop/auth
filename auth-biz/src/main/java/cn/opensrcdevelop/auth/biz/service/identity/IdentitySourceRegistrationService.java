package cn.opensrcdevelop.auth.biz.service.identity;

import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceRegistrationRequestDto;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceRegistrationResponseDto;
import cn.opensrcdevelop.auth.biz.dto.identity.UserBindingResponseDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.List;

public interface IdentitySourceRegistrationService extends IService<IdentitySourceRegistration> {

    IdentitySourceRegistration getByCode(String code);

    void removeIdentitySourceRegistrations(List<String> ids);

    void createIdentitySourceRegistration(IdentitySourceRegistrationRequestDto requestDto);

    void updateIdentitySourceRegistration(IdentitySourceRegistrationRequestDto requestDto);

    void removeIdentitySourceRegistration(String id);

    PageData<IdentitySourceRegistrationResponseDto> list(int page, int size, String keyword);

    IdentitySourceRegistrationResponseDto detail(String id);

    List<IdentitySourceRegistrationResponseDto> getEnabledRegistrations();

    List<IdentitySourceRegistrationResponseDto> getBoundRegistrations();

    UserBindingResponseDto bindUser(String registrationCode, HttpServletRequest request, HttpServletResponse response) throws IOException;

    void unbindUser(String registrationId);
}
