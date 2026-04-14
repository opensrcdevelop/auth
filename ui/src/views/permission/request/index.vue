<script lang="ts">
import requestTs from "./index";
export default requestTs;
</script>

<style lang="scss" scoped>
.permission-request-container {
  width: 100%;
  padding: 16px;
}

.page-header {
  margin-bottom: 16px;

  .title {
    font-size: 16px;
    font-weight: 500;
    color: var(--color-text-1);
    margin-bottom: 8px;
  }

  .info {
    font-size: 14px;
    color: var(--color-text-3);
  }
}

.filter-section {
  margin-bottom: 16px;
  padding: 12px 16px;
  background: var(--color-fill-lightest);
  border-radius: 4px;

  .filter-label {
    font-size: 14px;
    color: var(--color-text-2);
    margin-right: 12px;
  }
}
</style>

<template>
  <div class="permission-request-container">
    <!-- 页面标题 -->
    <div class="page-header">
      <div class="title">权限申请审批</div>
      <div class="info">
        查看和管理用户的权限申请，支持部分批准和部分拒绝操作。
      </div>
    </div>

    <!-- Tab 切换 -->
    <a-tabs
      :active-key="activeTab"
      @change="handleTabChange"
    >
      <!-- 待审批 Tab -->
      <a-tab-pane key="pending" title="待审批">
        <a-spin :loading="loading" style="width: 100%">
          <a-table
            :data="pendingList"
            :columns="columns"
            :bordered="false"
            :pagination="{
              ...pagination,
              showPageSize: true,
              showTotal: true,
            }"
            @page-change="handlePageChange"
            @page-size-change="handlePageSizeChange"
          >
            <template #columns>
              <a-table-column title="申请ID" dataIndex="requestId" :width="200">
                <template #cell="{ record }">
                  <copy-text :text="record.requestId" />
                </template>
              </a-table-column>
              <a-table-column title="申请时间" dataIndex="requestTime" :width="180" />
              <a-table-column title="申请理由" dataIndex="reason" ellipsis />
              <a-table-column title="权限数量" :width="100" align="center">
                <template #cell="{ record }">
                  {{ record.itemCount }}
                </template>
              </a-table-column>
              <a-table-column title="状态" :width="100" align="center">
                <template #cell="{ record }">
                  <a-tag :color="getStatusColor(record.status)">
                    {{ renderStatus(record.status) }}
                  </a-tag>
                </template>
              </a-table-column>
              <a-table-column title="操作" :width="100" align="center">
                <template #cell="{ record }">
                  <a-button
                    type="text"
                    size="small"
                    @click="handleViewDetail(record)"
                  >
                    查看详情
                  </a-button>
                </template>
              </a-table-column>
            </template>
          </a-table>
        </a-spin>
      </a-tab-pane>

      <!-- 全部申请 Tab -->
      <a-tab-pane key="all" title="全部申请">
        <a-spin :loading="loading" style="width: 100%">
          <!-- 状态筛选器（D-05） -->
          <div class="filter-section">
            <span class="filter-label">状态筛选：</span>
            <a-select
              v-model="selectedStatuses"
              :options="statusOptions"
              placeholder="请选择状态"
              multiple
              allow-clear
              :max-tag-count="3"
              style="width: 320px"
              @change="handleStatusFilterChange"
            />
          </div>

          <a-table
            :data="allList"
            :columns="columns"
            :bordered="false"
            :pagination="{
              ...pagination,
              showPageSize: true,
              showTotal: true,
            }"
            @page-change="handlePageChange"
            @page-size-change="handlePageSizeChange"
          >
            <template #columns>
              <a-table-column title="申请ID" dataIndex="requestId" :width="200">
                <template #cell="{ record }">
                  <copy-text :text="record.requestId" />
                </template>
              </a-table-column>
              <a-table-column title="申请时间" dataIndex="requestTime" :width="180" />
              <a-table-column title="申请理由" dataIndex="reason" ellipsis />
              <a-table-column title="权限数量" :width="100" align="center">
                <template #cell="{ record }">
                  {{ record.itemCount }}
                </template>
              </a-table-column>
              <a-table-column title="状态" :width="100" align="center">
                <template #cell="{ record }">
                  <a-tag :color="getStatusColor(record.status)">
                    {{ renderStatus(record.status) }}
                  </a-tag>
                </template>
              </a-table-column>
              <a-table-column title="操作" :width="100" align="center">
                <template #cell="{ record }">
                  <a-button
                    type="text"
                    size="small"
                    @click="handleViewDetail(record)"
                  >
                    查看详情
                  </a-button>
                </template>
              </a-table-column>
            </template>
          </a-table>
        </a-spin>
      </a-tab-pane>
    </a-tabs>
  </div>
</template>
