<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <div class="log-header">
      <div class="left">
        <div class="title">系统操作日志</div>
        <div class="info">对系统操作进行记录，提供审计。</div>
      </div>
    </div>
    <div class="log-filter">
      <a-space>
        <a-input-search
          :style="{ width: '240px' }"
          placeholder="用户名 / 用户ID / IP"
          v-model="searchKeyword"
          allow-clear
          @search="handleGetSysOperationLogs()"
          @clear="handleGetSysOperationLogs()"
        />
        <a-select
          :style="{ width: '120px' }"
          placeholder="操作类型"
          allow-clear
          v-model="operationType"
          @change="handleGetSysOperationLogs()"
        >
          <a-option
            v-for="(item, index) in operationTypes"
            :key="index"
            :value="item.value"
          >
            {{ item.label }}
          </a-option>
        </a-select>
        <a-select
          :style="{ width: '140px' }"
          placeholder="资源类型"
          allow-clear
          v-model="resourceType"
          @change="handleGetSysOperationLogs()"
        >
          <a-option
            v-for="(item, index) in resourceTypes"
            :key="index"
            :value="item.id"
          >
            {{ item.name }}
          </a-option>
        </a-select>
        <a-trigger trigger="click" :popup-offset="8">
          <a-button>
            <template #icon>
              <icon-settings />
            </template>
          </a-button>
          <template #content>
            <div class="column-filter-container">
              <div
                class="column-filter-item"
                v-for="(item, index) in columns"
                :key="index"
              >
                <a-checkbox
                  style="width: 100%"
                  v-model="item.visible"
                  :disabled="!item.editable"
                  >{{ item.label }}</a-checkbox
                >
              </div>
            </div>
          </template>
        </a-trigger>
        <a-button @click="handleGetSysOperationLogs()">
          <template #icon>
            <icon-refresh />
          </template>
        </a-button>
      </a-space>
      <a-range-picker
        position="br"
        value-format="YYYY-MM-DD HH:mm:ss"
        :disabledDate="(current) => getDayjs(current).isAfter(new Date())"
        :shortcuts="[
          {
            label: '近 7 天',
            value: () => [
              new Date(),
              getDayjs(new Date()).subtract(7, 'day').toDate(),
            ],
          },
          {
            label: '近 30 天',
            value: () => [
              new Date(),
              getDayjs(new Date()).subtract(30, 'day').toDate(),
            ],
          },
          {
            label: '近 90 天',
            value: () => [
              new Date(),
              getDayjs(new Date()).subtract(90, 'day').toDate(),
            ],
          },
          {
            label: '近 180 天',
            value: () => [
              new Date(),
              getDayjs(new Date()).subtract(180, 'day').toDate(),
            ],
          },
        ]"
        v-model="dateRange"
        @change="handleGetSysOperationLogs()"
      />
    </div>
    <div class="log-content">
      <a-table
        :data="sysOperationLogs"
        :bordered="false"
        :pagination="sysOperationLogsPagination.pagination"
        :scroll="{ y: '100%' }"
        :expandable="{ width: 30 }"
        row-key="id"
        @page-change="sysOperationLogsPagination.handlePageChange"
        @page-size-change="sysOperationLogsPagination.handlePageSizeChange"
      >
        <template #columns>
          <div v-for="(column, index) in columns" :key="index">
            <a-table-column
              v-if="column.visible"
              :title="column.label"
              :width="column.width as number"
              :ellipsis="column.ellipsis"
            >
              <template #cell="{ record }">
                <span
                  v-if="column.key === 'username'"
                  class="link"
                  @click="handleToUserDetail(record.userId)"
                >
                  {{ record[column.key] }}
                </span>
                <span
                  v-else-if="column.key === 'detail'"
                  v-html="record[column.key]"
                >
                </span>
                <span v-else-if="column.key === 'type'">
                  {{
                    operationTypes.find(
                      (item) => item.value === record[column.key]
                    )?.label
                  }}
                </span>
                <span
                  v-else-if="column.key === 'resourceId'"
                  class="link"
                  @click="handleToResourceDetail(record.resourceId)"
                >
                  {{
                    resourceTypes.find((item) => item.id === record[column.key])
                      ?.name
                  }}
                </span>
                <a-tag
                  v-else-if="column.key === 'result'"
                  :color="record[column.key] ? '#00b42a' : '#f53f3f'"
                >
                  {{ record[column.key] ? "成功" : "失败" }}
                </a-tag>
                <span v-else>
                  {{ record[column.key] }}
                </span>
              </template>
            </a-table-column>
          </div>
        </template>
        <template #expand-row="{ record }">
          <div class="all-info-container">
            <a-descriptions :column="3">
              <a-descriptions-item label="时间">
                <span>{{ record.time }}</span>
              </a-descriptions-item>
              <a-descriptions-item label="操作类型">
                <span>{{
                  operationTypes.find((item) => item.value === record.type)
                    ?.label
                }}</span>
              </a-descriptions-item>
              <a-descriptions-item label="资源类型">
                <span
                  class="link"
                  @click="handleToResourceDetail(record.resourceId)"
                  >{{
                    resourceTypes.find((item) => item.id === record.resourceId)
                      ?.name
                  }}</span
                >
              </a-descriptions-item>
              <a-descriptions-item :span="3">
                <template #label>
                  <span>操作详情</span>
                  <a-button
                    type="text"
                    size="mini"
                    style="margin-left: 6px"
                    v-if="record.type == 2 && record.result"
                    @click="handleGetObjChanges(record.id)"
                    >查询变更</a-button
                  >
                </template>
                <div v-html="record.detail" />
              </a-descriptions-item>
              <a-descriptions-item label="用户名">
                <span>{{ record.username }}</span>
              </a-descriptions-item>
              <a-descriptions-item label="用户 ID">
                <span>{{ record.userId }}</span>
              </a-descriptions-item>
              <a-descriptions-item label="请求 ID">
                <span>{{ record.requestId }}</span>
              </a-descriptions-item>
              <a-descriptions-item label="IP">
                <span>{{ record.ip }}</span>
              </a-descriptions-item>
              <a-descriptions-item label="IP 归属地">
                <span>{{ record.ipRegion }}</span>
              </a-descriptions-item>
              <a-descriptions-item label="设备类型">
                <span>{{ record.deviceType }}</span>
              </a-descriptions-item>
              <a-descriptions-item label="OS 类型">
                <span>{{ record.osType }}</span>
              </a-descriptions-item>
              <a-descriptions-item label="浏览器类型">
                <span>{{ record.browserType }}</span>
              </a-descriptions-item>
            </a-descriptions>
          </div>
        </template>
        <template #expand-icon="{ expanded }">
          <icon-right v-if="!expanded" />
          <icon-down v-if="expanded" />
        </template>
      </a-table>
    </div>

    <a-modal
      :visible="objChangesModalVisible"
      :footer="false"
      draggable
      :mask="false"
      :width="680"
      :modal-style="{
        border: '1px solid var(--color-neutral-3)'
      }"
      @cancel="objChangesModalVisible = false"
    >
      <template #title> 对象变更日志 </template>
      <a-table :data="objChanges" :pagination="false">
        <template #columns>
          <a-table-column title="No">
            <template #cell="{ rowIndex }">
              <span>{{ rowIndex + 1 }}</span>
            </template>
          </a-table-column>
          <a-table-column title="变更">
            <template #cell="{ record }">
              <span class="pre-wrap">{{ record }}</span>
            </template>
          </a-table-column>
        </template>
      </a-table>
    </a-modal>
  </div>
</template>
