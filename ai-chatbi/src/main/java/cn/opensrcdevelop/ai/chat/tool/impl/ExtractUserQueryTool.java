package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.ChatAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component(ExtractUserQueryTool.TOOL_NAME)
@RequiredArgsConstructor
public class ExtractUserQueryTool implements MethodTool {

    public static final String TOOL_NAME = "extract_user_query";

    private final ChatAgent chatAgent;

    @Tool(
            name = TOOL_NAME,
            description = "Used to extract user query from question"
    )
    public Response execute() {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();
        chatContext.setUserQuery(null);

        String question = chatContext.getQuestion();
        if (StringUtils.isEmpty(question)) {
            question = chatContext.getRawQuestion();
        }
        Map<String, Object> result = chatAgent.extractQuery(chatContext.getChatClient(), question);
        Boolean success = (Boolean) result.get("success");
        if (Boolean.TRUE.equals(success)) {
            String extractedQuery = (String) result.get("extracted_query");
            response.extractedQuery = extractedQuery;
            chatContext.setUserQuery(extractedQuery);
        } else {
            chatContext.setUserQuery(question);
        }

        response.setSuccess(success);
        response.setError((String) result.get("error"));
        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of extract user query")
        private Boolean success;

        @ToolParam(description = "The extracted query")
        private String extractedQuery;

        @ToolParam(description = "The error message if extract user query failed")
        private String error;
    }
}
