package cn.opensrcdevelop.ai.prompt;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
@ConfigurationProperties("ai.prompt")
@Getter
@Setter
public class PromptTemplate {

    private Map<String, Prompt> templates;


    @Getter
    @Setter
    public static class Prompt {

        private String system;

        private String user;
    }
}
