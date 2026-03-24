package cn.opensrcdevelop.ai.vectorstore.chroma;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
@ConfigurationProperties(prefix = "chroma")
public class ChromaConfigProperties {

    private String endpoint = "http://localhost:8000";

    private String tenantName = "default_tenant";

    private String databaseName = "default_database";

    private String collectionNamePrefix = "sample_sql_";

    private String apiKey;

    private String username;

    private String password;
}
