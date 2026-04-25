package cn.opensrcdevelop.ai.vectorstore.milvus;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
@ConfigurationProperties(prefix = "milvus")
public class MilvusConfigProperties {

    private String endpoint;

    private String username;

    private String password;

    private String databaseName = "default";
}
