package cn.opensrcdevelop.auth.biz.service.identity;

import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceProviderRequestDto;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceProviderResponseDto;
import cn.opensrcdevelop.auth.biz.dto.identity.IdentitySourceRegistrationResponseDto;
import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceProvider;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface IdentitySourceProviderService extends IService<IdentitySourceProvider> {

    void createIdentitySourceProvider(IdentitySourceProviderRequestDto requestDto);

    void updateIdentitySourceProvider(IdentitySourceProviderRequestDto requestDto);

    void removeIdentitySourceProvider(String providerId);

    PageData<IdentitySourceProviderResponseDto> list(int page, int size, String keyword);

    IdentitySourceProviderResponseDto detail(String providerId);

    List<IdentitySourceRegistrationResponseDto> registrations(String providerId);
}
