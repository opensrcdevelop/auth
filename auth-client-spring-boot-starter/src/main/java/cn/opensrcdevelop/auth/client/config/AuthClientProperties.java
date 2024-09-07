package cn.opensrcdevelop.auth.client.config;

import lombok.Data;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.util.StringUtils;

import java.util.HashMap;
import java.util.Map;

@ConfigurationProperties(prefix = "auth.client")
@Data
public class AuthClientProperties implements InitializingBean {

    private Map<String, ResourcePermission> authorize = new HashMap<>();

    private String issuer;

    @Override
    public void afterPropertiesSet() throws Exception {
        authorize.values().forEach(this::validateResourcePermission);
    }

    @Data
    public static class ResourcePermission {

        /** 资源 */
        private String resource;

        /** 权限 */
        private String permission;
    }

    private void validateResourcePermission(ResourcePermission resourcePermission) {
        if (!StringUtils.hasText(resourcePermission.resource)) {
            throw new IllegalStateException("Resource must not be empty.");
        }

        if (!StringUtils.hasText(resourcePermission.permission)) {
            throw new IllegalStateException("Permission must not be empty.");
        }
    }
}
