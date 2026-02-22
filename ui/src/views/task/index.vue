<script lang="ts">
import taskTs from "./index";

export default taskTs;
</script>

<template>
  <div class="task-center">
    <div class="task-header">
      <div class="left">
        <div class="title">任务列表</div>
        <div class="info">查看和管理异步任务的执行状态。</div>
      </div>
    </div>
    <div class="task-list">
      <div class="task-operation">
        <a-space>
          <a-select v-model="taskTypeFilter" placeholder="任务类型" allowClear style="width: 150px" :options="taskTypeOptions" />
          <a-select v-model="statusFilter" placeholder="任务状态" allowClear style="width: 150px" :options="statusOptions" />
          <a-button @click="handleSearch">
            <template #icon><icon-search /></template>
          </a-button>
          <a-button @click="handleReset">
            <template #icon><icon-refresh /></template>
          </a-button>
        </a-space>
      </div>

      <a-table
        :bordered="false"
        :data="taskList"
        :loading="loading"
        :pagination="pagination"
        :scroll="{ x: 800 }"
        row-key="taskId"
        @page-change="handlePageChange"
        @page-size-change="handlePageSizeChange"
      >
        <template #columns>
          <a-table-column title="任务类型" data-index="taskType" :width="120">
            <template #cell="{ record }">
              {{ getTaskTypeName(record.taskType) }}
            </template>
          </a-table-column>
          <a-table-column title="状态" data-index="status" :width="100">
            <template #cell="{ record }">
              <a-tag :color="getStatusColor(record.status)">
                {{ getStatusName(record.status) }}
              </a-tag>
            </template>
          </a-table-column>
          <a-table-column title="进度" data-index="progress" :width="150">
            <template #cell="{ record }">
              <a-progress :percent="(record.progress || 0) / 100" :stroke-width="6" style="width: 100px" />
            </template>
          </a-table-column>
          <a-table-column title="创建时间" data-index="createTime" :width="180">
            <template #cell="{ record }">
              {{ formatTime(record.createTime) }}
            </template>
          </a-table-column>
          <a-table-column title="操作" data-index="operations" :width="80" fixed="right">
            <template #cell="{ record }">
              <a-space>
                <a-button type="primary" size="mini" @click="handleView(record)">
                  查看
                </a-button>
              </a-space>
            </template>
          </a-table-column>
        </template>
      </a-table>
    </div>

    <!-- 任务详情抽屉 -->
    <a-drawer
      v-model:visible="detailVisible"
      title="任务详情"
      :width="'100%'"
      :footer="false"
    >
      <a-descriptions :column="1" bordered>
        <a-descriptions-item label="任务ID">
          {{ currentTask?.taskId }}
        </a-descriptions-item>
        <a-descriptions-item label="任务类型">
          {{ getTaskTypeName(currentTask?.taskType) }}
        </a-descriptions-item>
        <a-descriptions-item label="任务名称">
          {{ currentTask?.taskName }}
        </a-descriptions-item>
        <a-descriptions-item label="任务状态">
          <a-tag :color="getStatusColor(currentTask?.status)">
            {{ getStatusName(currentTask?.status) }}
          </a-tag>
        </a-descriptions-item>
        <a-descriptions-item label="进度">
          <a-progress :percent="(currentTask?.progress || 0) / 100" :stroke-width="6" style="width: 200px" />
        </a-descriptions-item>
        <a-descriptions-item label="创建时间">
          {{ formatTime(currentTask?.createTime) }}
        </a-descriptions-item>
        <a-descriptions-item label="开始时间">
          {{ formatTime(currentTask?.startTime) }}
        </a-descriptions-item>
        <a-descriptions-item label="结束时间">
          {{ formatTime(currentTask?.endTime) }}
        </a-descriptions-item>
        <a-descriptions-item label="耗时">
          {{ formatDuration(currentTask?.duration) }}
        </a-descriptions-item>
        <a-descriptions-item v-if="currentTask?.errorMessage" label="错误信息">
          <a-alert type="error" :message="currentTask?.errorMessage" />
        </a-descriptions-item>
        <!-- 用户导入结果 -->
        <a-descriptions-item v-if="currentTask?.taskType === 'USER_IMPORT' && parsedTaskParams?.fileName" label="上传文件">
          {{ parsedTaskParams?.fileName }}
        </a-descriptions-item>
        <a-descriptions-item v-if="currentTask?.taskType === 'USER_IMPORT' && (parsedTaskResult || currentTask?.taskResult)" label="导入统计" :span="2">
          <template v-if="parsedTaskResult">
            <a-descriptions :column="4" bordered size="small">
              <a-descriptions-item label="创建用户">
                {{ parsedTaskResult.createdCount || 0 }}
              </a-descriptions-item>
              <a-descriptions-item label="更新用户">
                {{ parsedTaskResult.updatedCount || 0 }}
              </a-descriptions-item>
              <a-descriptions-item label="删除用户">
                {{ parsedTaskResult.deletedCount || 0 }}
              </a-descriptions-item>
              <a-descriptions-item label="失败">
                <a-button v-if="parsedTaskResult.errors && parsedTaskResult.errors.length > 0" status="warning" size="small" @click="handleShowErrorDetail">
                  {{ parsedTaskResult.errors?.length }} 条
                </a-button>
                <span v-else>0</span>
              </a-descriptions-item>
            </a-descriptions>
          </template>
          <template v-else>
            <a-alert type="warning" message="任务结果解析失败">
              <template #content>
                <pre style="white-space: pre-wrap; word-break: break-all; max-height: 200px; overflow: auto">{{ currentTask?.taskResult }}</pre>
              </template>
            </a-alert>
          </template>
        </a-descriptions-item>
        <!-- 其他任务类型的结果展示 -->
        <a-descriptions-item v-if="currentTask?.taskResult && currentTask?.taskType !== 'USER_IMPORT' && currentTask?.taskType !== 'USER_EXPORT'" label="任务结果" :span="2">
          <pre style="white-space: pre-wrap; word-break: break-all">{{ currentTask?.taskResult }}</pre>
        </a-descriptions-item>
        <a-descriptions-item v-if="currentTask?.resultFilePath && currentTask?.taskType === 'USER_EXPORT'" label="操作" :span="2">
          <a-link @click="handleDownload(currentTask)">
            下载结果
          </a-link>
        </a-descriptions-item>
      </a-descriptions>
    </a-drawer>

    <!-- 导入错误详情弹窗 -->
    <a-modal
      v-model:visible="errorDetailVisible"
      title="导入错误详情"
      :footer="false"
      width="900px"
    >
      <a-alert v-if="!parsedTaskResult" type="warning" message="任务结果解析失败" style="margin-bottom: 12px">
        <template #content>
          <pre style="white-space: pre-wrap; word-break: break-all; max-height: 200px; overflow: auto">{{ currentTask?.taskResult }}</pre>
        </template>
      </a-alert>
      <a-table
        v-else-if="parsedTaskResult.errors && parsedTaskResult.errors.length > 0"
        :data="parsedTaskResult.errors"
        :pagination="false"
        :scroll="{ y: '60vh' }"
      >
        <template #columns>
          <a-table-column title="行号" data-index="row" :width="80" />
          <a-table-column title="列名" data-index="column" :width="100" />
          <a-table-column title="错误信息" data-index="message" />
        </template>
      </a-table>
      <a-empty v-else description="没有错误数据" />
    </a-modal>
  </div>
</template>

<style lang="scss" scoped>
@use "./index.scss";
</style>
