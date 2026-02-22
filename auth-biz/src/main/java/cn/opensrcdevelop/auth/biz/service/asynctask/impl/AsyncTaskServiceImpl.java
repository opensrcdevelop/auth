package cn.opensrcdevelop.auth.biz.service.asynctask.impl;

import cn.opensrcdevelop.auth.biz.dto.asynctask.AsyncTaskResponseDto;
import cn.opensrcdevelop.auth.biz.entity.asynctask.AsyncTask;
import cn.opensrcdevelop.auth.biz.enums.AsyncTaskStatus;
import cn.opensrcdevelop.auth.biz.mapper.asynctask.AsyncTaskMapper;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

/**
 * 异步任务服务实现
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AsyncTaskServiceImpl implements AsyncTaskService {

    private final AsyncTaskMapper asyncTaskMapper;

    @Override
    public String createTask(String taskType, String taskName, String taskParams, String userId) {
        String taskId = CommonUtil.getUUIDV7String();

        AsyncTask task = new AsyncTask();
        task.setTaskId(taskId);
        task.setTaskType(taskType);
        task.setTaskName(taskName);
        task.setTaskParams(taskParams);
        task.setUserId(userId);
        task.setStatus(AsyncTaskStatus.PENDING.getCode());
        task.setProgress(0);

        asyncTaskMapper.insert(task);

        log.info("创建异步任务成功: taskId={}, taskType={}, taskName={}, userId={}", taskId, taskType, taskName, userId);

        return taskId;
    }

    @Override
    public void startTask(String taskId) {
        updateTaskStatus(taskId, AsyncTaskStatus.RUNNING.getCode());

        AsyncTask task = asyncTaskMapper.selectById(taskId);
        if (task != null) {
            task.setStartTime(LocalDateTime.now());
            asyncTaskMapper.updateById(task);
        }

        log.info("任务开始执行: taskId={}", taskId);
    }

    @Override
    public void completeTaskSuccess(String taskId, String taskResult, String resultFilePath, String resultFileName) {
        AsyncTask task = asyncTaskMapper.selectById(taskId);
        if (task == null) {
            log.warn("任务不存在: taskId={}", taskId);
            return;
        }

        task.setStatus(AsyncTaskStatus.SUCCESS.getCode());
        task.setTaskResult(taskResult);
        task.setResultFilePath(resultFilePath);
        task.setResultFileName(resultFileName);
        task.setEndTime(LocalDateTime.now());
        task.setProgress(100);

        // 计算执行耗时
        if (task.getStartTime() != null) {
            task.setDuration(Duration.between(task.getStartTime(), task.getEndTime()).toMillis());
        }

        asyncTaskMapper.updateById(task);

        log.info("任务执行成功: taskId={}, duration={}ms", taskId, task.getDuration());
    }

    @Override
    public void completeTaskFailed(String taskId, String errorMessage) {
        AsyncTask task = asyncTaskMapper.selectById(taskId);
        if (task == null) {
            log.warn("任务不存在: taskId={}", taskId);
            return;
        }

        task.setStatus(AsyncTaskStatus.FAILED.getCode());
        task.setErrorMessage(errorMessage);
        task.setEndTime(LocalDateTime.now());

        // 计算执行耗时
        if (task.getStartTime() != null) {
            task.setDuration(Duration.between(task.getStartTime(), task.getEndTime()).toMillis());
        }

        asyncTaskMapper.updateById(task);

        log.error("任务执行失败: taskId={}, error={}", taskId, errorMessage);
    }

    @Override
    public void updateProgress(String taskId, Integer progress) {
        AsyncTask task = asyncTaskMapper.selectById(taskId);
        if (task != null) {
            task.setProgress(progress);
            asyncTaskMapper.updateById(task);
        }
    }

    @Override
    public void cancelTask(String taskId) {
        updateTaskStatus(taskId, AsyncTaskStatus.CANCELLED.getCode());

        log.info("任务已取消: taskId={}", taskId);
    }

    @Override
    public AsyncTaskResponseDto getTaskDetailById(String taskId) {
        AsyncTask entity = asyncTaskMapper.selectById(taskId);
        if (entity == null) {
            return null;
        }
        return convertToDetailDto(entity);
    }

    @Override
    public PageData<AsyncTaskResponseDto> listTasks(int page, int pageSize, String taskType, String status,
            String userId) {
        LambdaQueryWrapper<AsyncTask> wrapper = new LambdaQueryWrapper<>();

        if (StringUtils.isNotBlank(taskType)) {
            wrapper.eq(AsyncTask::getTaskType, taskType);
        }
        if (StringUtils.isNotBlank(status)) {
            wrapper.eq(AsyncTask::getStatus, status);
        }
        if (StringUtils.isNotBlank(userId)) {
            wrapper.eq(AsyncTask::getUserId, userId);
        }

        wrapper.orderByDesc(AsyncTask::getCreateTime);

        IPage<AsyncTask> pageResult = asyncTaskMapper.selectPage(
                new Page<>(page, pageSize), wrapper);

        // 转换为列表 DTO（只设置列表需要的字段）
        List<AsyncTaskResponseDto> dtoList = pageResult.getRecords().stream()
                .map(this::convertToListDto)
                .collect(Collectors.toList());

        PageData<AsyncTaskResponseDto> pageData = new PageData<>();
        pageData.setList(dtoList);
        pageData.setTotal(pageResult.getTotal());
        pageData.setCurrent((long) page);
        pageData.setSize((long) pageSize);
        pageData.setPages(pageResult.getPages());
        return pageData;
    }

    /**
     * 将 AsyncTask 实体转换为列表 DTO（只包含列表需要的字段）
     */
    private AsyncTaskResponseDto convertToListDto(AsyncTask entity) {
        AsyncTaskResponseDto dto = new AsyncTaskResponseDto();
        dto.setTaskId(entity.getTaskId());
        dto.setTaskType(entity.getTaskType());
        dto.setTaskName(entity.getTaskName());
        dto.setStatus(entity.getStatus());
        dto.setProgress(entity.getProgress());
        dto.setCreateTime(entity.getCreateTime());
        return dto;
    }

    /**
     * 将 AsyncTask 实体转换为详情 DTO
     */
    private AsyncTaskResponseDto convertToDetailDto(AsyncTask entity) {
        AsyncTaskResponseDto dto = new AsyncTaskResponseDto();
        dto.setTaskId(entity.getTaskId());
        dto.setTaskType(entity.getTaskType());
        dto.setTaskName(entity.getTaskName());
        dto.setStatus(entity.getStatus());
        dto.setTaskParams(entity.getTaskParams());
        dto.setTaskResult(entity.getTaskResult());
        dto.setResultFilePath(entity.getResultFilePath());
        dto.setResultFileName(entity.getResultFileName());
        dto.setErrorMessage(entity.getErrorMessage());
        dto.setProgress(entity.getProgress());
        dto.setStartTime(entity.getStartTime());
        dto.setEndTime(entity.getEndTime());
        dto.setDuration(entity.getDuration());
        dto.setUserId(entity.getUserId());
        dto.setCreateTime(entity.getCreateTime());
        dto.setUpdateTime(entity.getUpdateTime());
        return dto;
    }

    @Override
    public void updateTaskStatus(String taskId, String status) {
        AsyncTask task = asyncTaskMapper.selectById(taskId);
        if (task != null) {
            task.setStatus(status);
            asyncTaskMapper.updateById(task);
        }
    }

    @Override
    public long countRunningTasks(String taskType) {
        LambdaQueryWrapper<AsyncTask> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(AsyncTask::getTaskType, taskType)
                .eq(AsyncTask::getStatus, AsyncTaskStatus.RUNNING.getCode());
        return asyncTaskMapper.selectCount(wrapper);
    }
}
