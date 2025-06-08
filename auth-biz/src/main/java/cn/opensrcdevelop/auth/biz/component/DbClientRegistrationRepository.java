package cn.opensrcdevelop.auth.biz.component;

import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import cn.opensrcdevelop.auth.biz.mapper.identity.IdentitySourceRegistrationMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
@RequiredArgsConstructor
public class DbClientRegistrationRepository implements ClientRegistrationRepository {

    private final IdentitySourceRegistrationMapper identitySourceRegistrationMapper;

    @Override
    public ClientRegistration findByRegistrationId(String registrationId) {
        IdentitySourceRegistration identitySourceRegistration = identitySourceRegistrationMapper.getRegistrationByCode(registrationId);
        if (Objects.isNull(identitySourceRegistration) || Boolean.FALSE.equals(identitySourceRegistration.getEnabled())) {
            return null;
        }
        return identitySourceRegistration.toClientRegistration();
    }
}
