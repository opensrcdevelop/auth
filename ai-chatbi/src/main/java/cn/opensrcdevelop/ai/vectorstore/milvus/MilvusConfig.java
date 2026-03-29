package cn.opensrcdevelop.ai.vectorstore.milvus;

import io.milvus.v2.client.ConnectConfig;
import io.milvus.v2.client.MilvusClientV2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationPropertiesScan
@ConditionalOnProperty(name = "vectorstore.type", havingValue = "milvus")
public class MilvusConfig {

    @Bean
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
