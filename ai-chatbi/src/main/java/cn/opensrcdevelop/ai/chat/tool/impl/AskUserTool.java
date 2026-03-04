package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import java.util.List;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

@Component(AskUserTool.TOOL_NAME)
@RequiredArgsConstructor
public class AskUserTool implements MethodTool {

    public static final String TOOL_NAME = "ask_user";

    @Tool(name = TOOL_NAME, description = "当 AI 无法直接回答问题或缺少必要信息时，向用户提问获取更多信息。适用于：1. 缺少关键筛选条件；2. 用户意图不明确；3. 需要用户从多个选项中选择（支持自定义输入）。支持同时传递多个问题，用户可通过 tab 切换不同问题。")
    public Response execute(@ToolParam(description = "请求参数") Request request) {
        Response response = new Response();
        response.setSuccess(true);
        response.setQuestions(request.getQuestions());
        response.setIsAskUser(true);
        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Request {

        @ToolParam(description = "问题列表，支持同时传递多个问题（使用 tab 切换）", required = true)
        private List<Question> questions;
    }

    @Data
    public static class Question {

        @ToolParam(description = "问题 ID，用于关联用户回答", required = true)
        private String id;

        @ToolParam(description = "需要询问的问题", required = true)
        private String question;

        @ToolParam(description = "问题类型：TEXT（文本输入）、SELECT（单选，支持自定义输入）、MULTI_SELECT（多选）、DATE（日期选择）、NUMBER（数字输入）", required = false)
        private String questionType;

        @ToolParam(description = "选项列表，当 questionType 为 SELECT 或 MULTI_SELECT 时必填", required = false)
        private List<String> options;

        @ToolParam(description = "是否必填，默认为 true", required = false)
        private Boolean required;

        @ToolParam(description = "上下文信息，帮助用户理解问题", required = false)
        private String context;

        @ToolParam(description = "问题标题（简短）", required = false)
        private String title;
    }

    @Data
    public static class Response {

        @ToolParam(description = "是否成功")
        private Boolean success;

        @ToolParam(description = "问题列表")
        private List<Question> questions;

        @ToolParam(description = "标记需要向用户询问")
        private Boolean isAskUser;
    }
}
