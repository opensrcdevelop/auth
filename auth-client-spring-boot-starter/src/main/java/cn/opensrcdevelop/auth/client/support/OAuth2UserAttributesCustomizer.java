package cn.opensrcdevelop.auth.client.support;

import org.springframework.security.config.Customizer;

public interface OAuth2UserAttributesCustomizer extends Customizer<OAuth2UserAttributes> {

    static OAuth2UserAttributesCustomizer withDefaults() {
        return t -> {
        };
    }
}
