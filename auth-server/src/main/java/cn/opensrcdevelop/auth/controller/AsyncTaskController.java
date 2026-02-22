package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.asynctask.AsyncTaskResponseDto;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskService;
import cn.opensrcdevelop.auth.biz.service.asynctask.storage.StorageService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.annoation.NoRestResponse;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.response.PageData;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

/**
 * 异步任务控制器
 */
@Tag(name = "API-AsyncTask", description = "接口-异步任务管理")
@RestController
@RestResponse
@RequestMapping("/task")
@RequiredArgsConstructor
public class AsyncTaskController {

    private final AsyncTaskService asyncTaskService;
    private final StorageService storageService;

    @Operation(summary = "查询任务详情", description = "根据任务ID查询任务详情")
    @GetMapping("/{taskId}")
    public AsyncTaskResponseDto getTaskDetail(@PathVariable String taskId) {
        return asyncTaskService.getTaskDetailById(taskId);
    }

    @Operation(summary = "分页查询任务列表", description = "分页查询当前用户的任务列表")
    @GetMapping("/list")
    public PageData<AsyncTaskResponseDto> listTasks(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int pageSize,
            @RequestParam(required = false) String taskType,
            @RequestParam(required = false) String status) {
        // 从 SecurityContext 自动获取当前用户ID，强制过滤该用户创建的任务
        String userId = AuthUtil.getCurrentUserId();
        return asyncTaskService.listTasks(page, pageSize, taskType, status, userId);
    }

    @Operation(summary = "下载任务结果文件", description = "下载任务结果文件")
    @GetMapping("/{taskId}/download")
    @NoRestResponse
    public void downloadTaskResult(@PathVariable String taskId, HttpServletResponse response) throws IOException {
        AsyncTaskResponseDto task = asyncTaskService.getTaskDetailById(taskId);
        if (task == null) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND, "任务不存在");
            return;
        }
        if (task.getResultFilePath() == null) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND, "结果文件不存在");
            return;
        }
        byte[] data = storageService.read(task.getResultFilePath());
        response.setContentType(CommonConstants.EXCEL_CONTENT_TYPE);
        response.setHeader("Content-Disposition", "attachment; filename=\"" + task.getResultFileName() + "\"");
        response.getOutputStream().write(data);
        response.getOutputStream().flush();
    }
}
