package cn.opensrcdevelop.auth.client.support;

import org.springframework.http.client.ClientHttpRequest;

public interface PermissionVerifyRequestCustomizer {

    void customize(ClientHttpRequest httpRequest);
}
