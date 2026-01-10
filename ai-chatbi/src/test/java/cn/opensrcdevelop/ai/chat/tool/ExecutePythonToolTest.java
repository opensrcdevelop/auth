package cn.opensrcdevelop.ai.chat.tool;

import static org.junit.jupiter.api.Assertions.assertTrue;

import cn.opensrcdevelop.ai.chat.tool.impl.ExecutePythonTool;
import java.util.List;
import org.junit.jupiter.api.Test;

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
