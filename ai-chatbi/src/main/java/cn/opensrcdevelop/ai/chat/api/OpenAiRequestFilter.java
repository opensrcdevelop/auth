package cn.opensrcdevelop.ai.chat.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.MediaType;
import org.springframework.http.client.reactive.ClientHttpRequest;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ClientRequest;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.ExchangeFilterFunction;
import org.springframework.web.reactive.function.client.ExchangeFunction;
import reactor.core.publisher.Mono;

/**
 * OpenAI 请求过滤器，拦截并处理 ChatCompletionRequest
 */
@Slf4j
public class OpenAiRequestFilter implements ExchangeFilterFunction {

    private final ObjectMapper objectMapper;

    public OpenAiRequestFilter() {
        this.objectMapper = new ObjectMapper();
    }

    @NotNull
    @Override
    public Mono<ClientResponse> filter(@NotNull ClientRequest request, @NotNull ExchangeFunction next) {
        // 检查是否是 JSON 请求
        MediaType contentType = request.headers().getContentType();
        if (contentType == null || !MediaType.APPLICATION_JSON.includes(contentType)) {
            return next.exchange(request);
        }

        log.info("【OpenAI Request Filter】拦截到请求: {} {}", request.method(), request.url());

        try {
            // 通过反射获取请求体内容
            String bodyString = parseRequestBody(request.body());
            if (bodyString != null && !bodyString.isEmpty()) {
                log.info("【OpenAI Request Filter】原始请求体:\n{}", bodyString);

                // 处理 ASSISTANT 消息中的 reasoning_content
                String modifiedBody = processAssistantMessages(bodyString);

                // 打印编辑后的请求体
                log.info("【OpenAI Request Filter】编辑后的请求体:\n{}", modifiedBody);

                // 构建修改后的请求
                ClientRequest modifiedRequest = ClientRequest
                        .from(request)
                        .header("Content-Type", MediaType.APPLICATION_JSON_VALUE)
                        .body(BodyInserters.fromValue(modifiedBody))
                        .build();

                return next.exchange(modifiedRequest);
            }
        } catch (Exception e) {
            log.error("处理 OpenAI 请求时发生异常", e);
        }

        // 如果出现任何问题，直接继续执行原请求
        return next.exchange(request);
    }

    /**
     * 通过反射获取请求体内容
     */
    @SuppressWarnings("all")
    private String parseRequestBody(BodyInserter<?, ? super ClientHttpRequest> bodyInserter) {
        Class<? extends BodyInserter> clazz = bodyInserter.getClass();
        try {
            Field data = clazz.getDeclaredField("arg$1");
            data.setAccessible(true);
            Object result = data.get(bodyInserter);
            data.setAccessible(false);
            if (result instanceof String str) {
                return str;
            } else if (result instanceof byte[] bytes) {
                return new String(bytes, StandardCharsets.UTF_8);
            } else if (result != null) {
                return objectMapper.writeValueAsString(result);
            }
        } catch (Exception e) {
            log.debug("无法通过反射获取请求体内容: {}", e.getMessage());
        }
        return null;
    }

    /**
     * 处理 ASSISTANT 消息，将 reasoning_content 设置为空字符串
     */
    private String processAssistantMessages(String requestBody) throws IOException {
        JsonNode rootNode = objectMapper.readTree(requestBody);

        if (!rootNode.has("messages") || !rootNode.get("messages").isArray()) {
            return requestBody;
        }

        ArrayNode messagesArray = (ArrayNode) rootNode.get("messages");

        for (JsonNode messageNode : messagesArray) {
            if (!messageNode.isObject()) {
                continue;
            }

            ObjectNode messageObj = (ObjectNode) messageNode;

            // 检查 role 是否为 "assistant"
            if (!messageNode.has("role") || !"assistant".equals(messageNode.get("role").asText())) {
                continue;
            }

            // 如果存在 tool_calls 字段，将 reasoning_content 设置为空字符串
            if (messageObj.has("tool_calls")) {
                messageObj.put("reasoning_content", "");
            }
        }

        return objectMapper.writeValueAsString(rootNode);
    }
}
