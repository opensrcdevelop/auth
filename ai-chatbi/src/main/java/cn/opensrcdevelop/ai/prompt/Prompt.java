package cn.opensrcdevelop.ai.prompt;

import cn.opensrcdevelop.ai.util.PromptTemplateUtil;

import java.util.HashMap;
import java.util.Map;


public class Prompt {

    private final Map<String, Object> params;

    private final String system;

    private final String user;

    public Prompt(String system, String user) {
        this.system = system;
        this.user = user;
        this.params = new HashMap<>();
    }

    public Prompt param(String key, Object value) {
        this.params.put(key, value);
        return this;
    }

    public String buildSystemPrompt() {
        return PromptTemplateUtil.getPrompt(this.system, this.params);
    }

    public String buildUserPrompt() {
        return PromptTemplateUtil.getPrompt(this.user, this.params);
    }
}
