package cn.opensrcdevelop.ai.vectorstore.chroma;

import org.springframework.ai.chroma.api.ChromaApi;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.StringUtils;

@Configuration
@ConfigurationPropertiesScan
@ConditionalOnProperty(name = "vectorstore.type", havingValue = "chroma")
public class ChromaConfig {

    @Bean
    public ChromaApi chromaApi(ChromaConfigProperties properties) {
        ChromaApi api = ChromaApi.builder()
                .baseUrl(properties.getEndpoint())
                .build();

        if (StringUtils.hasText(properties.getApiKey())) {
            api.withKeyToken(properties.getApiKey());
        } else if (StringUtils.hasText(properties.getUsername())) {
            api.withBasicAuthCredentials(properties.getUsername(), properties.getPassword());
        }

        return api;
    }
}
