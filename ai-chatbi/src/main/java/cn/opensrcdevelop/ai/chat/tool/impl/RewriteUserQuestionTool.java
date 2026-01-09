package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.ChatAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component(RewriteUserQuestionTool.TOOL_NAME)
@RequiredArgsConstructor
public class RewriteUserQuestionTool implements MethodTool {

    public static final String TOOL_NAME = "rewrite_user_question";

    private final ChatAgent chatAgent;

    @Tool(
            name = TOOL_NAME,
            description = "Used to rewrite user question"
    )
    public Response execute(@ToolParam(description = "The request to rewrite user question") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();
        chatContext.setQuestion(null);

        String rawQuestion = chatContext.getRawQuestion();
        Map<String, Object> result = chatAgent.rewriteUserQuestion(chatContext.getChatClient(), rawQuestion, request.instruction);
        Boolean success = (Boolean) result.get("success");
        if (Boolean.TRUE.equals(success)) {
            String rewriteQuestion = (String) result.get("rewritten_question");
            chatContext.setQuestion(rewriteQuestion);
            response.setRewrittenQuestion(rewriteQuestion);
        } else {
            chatContext.setQuestion(rawQuestion);
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
    public static class Request {

        @ToolParam(description = "The instruction to rewrite user question", required = false)
        private String instruction;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of rewrite user question")
        private Boolean success;

        @ToolParam(description = "The rewritten question")
        private String rewrittenQuestion;

        @ToolParam(description = "The error message if rewrite user question failed")
        private String error;
    }
}
