package cn.opensrcdevelop.ai.chat.tool;

import cn.opensrcdevelop.ai.chat.tool.impl.ExecutePythonTool;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

class ExecutePythonToolTest {

    @Test
    void executePythonTool() {
        ExecutePythonTool executePythonTool = new ExecutePythonTool();

        ExecutePythonTool.Request request = new ExecutePythonTool.Request();
        request.setScript("print('hello world')");
        request.setPackages(List.of("numpy", "pandas", "scipy"));

        ExecutePythonTool.Response response = executePythonTool.execute(request);
        assertTrue(response.getSuccess());
    }
}
