package cn.opensrcdevelop.ai.chat.tool;

import org.springframework.ai.tool.ToolCallback;

public interface MethodTool {

    String toolName();

    ToolCallback[] getToolCallbacks();
}
