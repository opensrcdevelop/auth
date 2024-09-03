package cn.opensrcdevelop.tenant.component;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "multi.tenant")
@Data
public class MultiTenantProperties {

    private String dbPrefix;

    private String defaultTenant;

    private String flywayLocation;

    private Db db;

    private HikariCpConfig hikariCpConfig;

    @Data
    public static class Db {
        private String baseUrl;

        private String username;

        private String password;

        private String driverClassName;
    }

    @Data
    public static class HikariCpConfig {
        private Long connectionTimeout;

        private Long idleTimeout;

        private Integer maximumPoolSize;

        private Integer minimumIdle;

        private Boolean isAutoCommit;

        private Long maxLifetime;
    }
}
