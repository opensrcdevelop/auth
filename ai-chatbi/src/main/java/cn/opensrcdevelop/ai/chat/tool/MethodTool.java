package cn.opensrcdevelop.ai.chat.tool;

import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.method.MethodToolCallbackProvider;

public interface MethodTool {

    String toolName();

    default ToolCallback[] getToolCallbacks() {
        return MethodToolCallbackProvider.builder().toolObjects(this).build().getToolCallbacks();
    }
}
