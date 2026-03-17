package cn.opensrcdevelop.ai.vectorstore.milvus;

import io.milvus.v2.client.ConnectConfig;
import io.milvus.v2.client.MilvusClientV2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MilvusConfig {

    @Bean
    @ConfigurationProperties(prefix = "milvus.endpoint")
    public MilvusClientV2 milvusClient(MilvusConfigProperties milvusConfigProperties) {
        ConnectConfig.ConnectConfigBuilder builder = ConnectConfig.builder();
        builder.uri(milvusConfigProperties.getEndpoint());

        if (StringUtils.isNotBlank(milvusConfigProperties.getUsername())) {
            builder.username(milvusConfigProperties.getUsername());
        }

        if (StringUtils.isNotBlank(milvusConfigProperties.getPassword())) {
            builder.password(milvusConfigProperties.getPassword());
        }
        if (StringUtils.isNotBlank(milvusConfigProperties.getDatabaseName())) {
            builder.dbName(milvusConfigProperties.getDatabaseName());
        }

        return new MilvusClientV2(builder.build());
    }
}
