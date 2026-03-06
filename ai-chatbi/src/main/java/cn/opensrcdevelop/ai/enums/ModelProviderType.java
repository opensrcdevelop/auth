package cn.opensrcdevelop.ai.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ModelProviderType {

    OPENAI("OpenAI"),
    OLLAMA("Ollama"),
    ANTHROPIC("Anthropic"),;

    private final String displayName;
}
