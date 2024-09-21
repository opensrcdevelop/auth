package cn.opensrcdevelop.auth.client.support;

import org.springframework.security.config.Customizer;

public interface OAuth2AttributesCustomizer extends Customizer<OAuth2Attributes> {

    static OAuth2AttributesCustomizer withDefaults() {
        return t -> {
        };
    }
}
