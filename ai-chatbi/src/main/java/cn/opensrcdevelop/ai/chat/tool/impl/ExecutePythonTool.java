package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

@Component(ExecutePythonTool.TOOL_NAME)
@Slf4j
public class ExecutePythonTool implements MethodTool {

    public static final String TOOL_NAME = "execute_python_script";

    private static final String PYTHON_COMMAND = "python3";
    private static final String PYTHON_SCRIPT_NAME = "python_script_%s";
    private static final String PYTHON_SCRIPT_EXT = ".py";
    private static final String VENV_NAME_PREFIX = "ai_chat_venv_";
    private static final int DEFAULT_TIMEOUT_MINUTES = 3;

    @Tool(
            name = ExecutePythonTool.TOOL_NAME,
            description = "Used to execute Python scripts and return the results of Python script execution"
    )
    @SuppressWarnings({"unused", "java:S3776"})
    public Response execute(@ToolParam(description = "Request to execute Python script") Request request) {
        log.info("Execute Python script: {}", CommonUtil.serializeObject(request));
        String venvDir = Path.of(System.getProperty("java.io.tmpdir"), VENV_NAME_PREFIX + System.currentTimeMillis()).toString();
        Response response = new Response();
        File tempScriptFile = null;
        Process process = null;

        try (
                StringWriter stdoutWriter = new StringWriter();
                StringWriter stderrWriter = new StringWriter()
        ) {
            // 1. 创建 Python 虚拟环境
            Response venvResponse = createVirtualEnvironment(venvDir);
            if (!venvResponse.getSuccess()) {
                return venvResponse;
            }
            log.info("Virtual environment {} created successfully", venvDir);

            // 2. 检查是否需要安装包
            if (CollectionUtils.isNotEmpty(request.packages)) {
                log.info("Installing required packages: {}", request.getPackages());
                Response installResponse = installPackages(venvDir, request.getPackages());
                if (!installResponse.getSuccess()) {
                    log.error("Package installation failed: {}", installResponse.getResult());
                    return installResponse;
                }
                log.info("Package installation completed successfully");
            }

            // 3. 创建临时文件存储 Python 脚本
            tempScriptFile = File.createTempFile(PYTHON_SCRIPT_NAME.formatted(System.currentTimeMillis()), PYTHON_SCRIPT_EXT);
            try (FileWriter writer = new FileWriter(tempScriptFile)) {
                writer.write(request.getScript());
            }

            // 4. 使用虚拟环境执行 Python 脚本
            process = Runtime.getRuntime().exec(new String[]{Path.of(venvDir, "bin", "python").toString(), tempScriptFile.getAbsolutePath()});

            // 5. 读取标准输出
            try (
                    BufferedReader stdoutReader = new BufferedReader(new InputStreamReader(process.getInputStream()));
                    BufferedReader stderrReader = new BufferedReader(new InputStreamReader(process.getErrorStream()))
            ) {
                CompletableFuture<Void> stdoutFuture = CompletableFuture.runAsync(() -> {
                    try {
                        stdoutReader.transferTo(stdoutWriter);
                    } catch (IOException e) {
                        stderrWriter.write("error reading stdout: " + e.getMessage());
                    }
                });
                CompletableFuture<Void> stderrFuture = CompletableFuture.runAsync(() -> {
                    try {
                        stderrReader.transferTo(stderrWriter);
                    } catch (IOException e) {
                        stderrWriter.write("error reading stderr: " + e.getMessage());
                    }
                });

                boolean completed = process.waitFor(DEFAULT_TIMEOUT_MINUTES, TimeUnit.MINUTES);
                if (!completed) {
                    process.destroy();
                    if (process.isAlive()) {
                        process.destroyForcibly();
                    }
                    response.setSuccess(false);
                    response.setResult("execute python script timeout");
                    return response;
                }
                CompletableFuture.allOf(stdoutFuture, stderrFuture).join();
            }

            // 6. 处理执行结果
            int exitCode = process.exitValue();
            String stdout = stdoutWriter.toString();
            String stderr = stderrWriter.toString();
            if (exitCode != 0) {
                response.setSuccess(false);
                response.setResult(stderr);
            } else {
                response.setSuccess(true);
                response.setResult(stdout);
            }
        } catch (Exception ex) {
            log.error("execute python script error", ex);
            if (ex instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
            response.setSuccess(false);
            response.setResult("execute python script error: " + ex.getMessage());
        } finally {
            if (process != null && process.isAlive()) {
                process.destroyForcibly();
            }

            try {
                FileUtils.deleteDirectory(new File(venvDir));
                if (tempScriptFile != null) {
                    Files.deleteIfExists(tempScriptFile.toPath());
                }
            } catch (IOException e) {
                log.error("delete temp python script error", e);
            }
        }
        log.info("Execute Python script result: {}", CommonUtil.serializeObject(response));
        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    private Response installPackages(String venvDir, List<String> packages) {
        Response response = new Response();
        
        try {
            ProcessBuilder pb = new ProcessBuilder(Path.of(venvDir, "bin", "pip").toString(), "install");
            packages.forEach(pb.command()::add);
            pb.redirectErrorStream(true);
            
            Process process = pb.start();
            
            try (StringWriter outputWriter = new StringWriter();
                 BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                
                CompletableFuture<Void> readFuture = CompletableFuture.runAsync(() -> {
                    try {
                        reader.transferTo(outputWriter);
                    } catch (IOException e) {
                        log.error("Error reading pip install output", e);
                    }
                });

                boolean completed = process.waitFor(DEFAULT_TIMEOUT_MINUTES, TimeUnit.MINUTES);
                if (!completed) {
                    process.destroy();
                    if (process.isAlive()) {
                        process.destroyForcibly();
                    }
                    response.setSuccess(false);
                    response.setResult("Package installation timeout");
                    return response;
                }

                readFuture.join();
                
                int exitCode = process.exitValue();
                String output = outputWriter.toString();
                
                if (exitCode != 0) {
                    response.setSuccess(false);
                    response.setResult("Package installation failed: " + output);
                } else {
                    response.setSuccess(true);
                    response.setResult("Packages installed successfully: " + output);
                }
            }
            
        } catch (Exception ex) {
            log.error("Package installation error", ex);
            if (ex instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
            response.setSuccess(false);
            response.setResult("Package installation error: " + ex.getMessage());
        }

        response.setSuccess(true);
        return response;
    }

    private Response createVirtualEnvironment(String venvDir) {
        Response response = new Response();
        
        try {
            ProcessBuilder pb = new ProcessBuilder(PYTHON_COMMAND, "-m", "venv", venvDir);
            pb.redirectErrorStream(true);
            
            Process process = pb.start();
            
            try (StringWriter outputWriter = new StringWriter();
                 BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                
                CompletableFuture<Void> readFuture = CompletableFuture.runAsync(() -> {
                    try {
                        reader.transferTo(outputWriter);
                    } catch (IOException e) {
                        log.error("Error reading venv creation output", e);
                    }
                });

                boolean completed = process.waitFor(DEFAULT_TIMEOUT_MINUTES, TimeUnit.MINUTES);
                if (!completed) {
                    process.destroy();
                    if (process.isAlive()) {
                        process.destroyForcibly();
                    }
                    response.setSuccess(false);
                    response.setResult("Virtual environment creation timeout");
                    return response;
                }
                
                readFuture.join();
                
                int exitCode = process.exitValue();
                String output = outputWriter.toString();
                
                if (exitCode != 0) {
                    response.setSuccess(false);
                    response.setResult("Virtual environment creation failed: " + output);
                } else {
                    response.setSuccess(true);
                    response.setResult("Virtual environment created successfully: " + output);
                }
            }
            
        } catch (Exception ex) {
            log.error("Virtual environment creation error", ex);
            if (ex instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
            response.setSuccess(false);
            response.setResult("Virtual environment creation error: " + ex.getMessage());
        }

        response.setSuccess(true);
        return response;
    }

    @Data
    public static class Request {

        @ToolParam(description = "Python script")
        private String script;

        @ToolParam(description = "Python script execution required packages")
        private List<String> packages;
    }

    @Data
    public static class Response {

        @ToolParam(description = "Python script execution success")
        private Boolean success;

        @ToolParam(description = "Python script execution result")
        public String result;
    }
}