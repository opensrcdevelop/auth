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
                    @click="handleViewDetail(record.requestId)"
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
                    @click="handleViewDetail(record.requestId)"
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

    <!-- 详情弹窗 -->
    <a-modal
      v-model:visible="detailModalVisible"
      title="申请详情"
      :width="700"
      :footer="null"
      @cancel="handleCloseDetail"
    >
      <a-spin :loading="detailLoading" style="width: 100%">
        <a-form v-if="requestDetail" layout="vertical">
          <!-- 申请信息 -->
          <a-divider orientation="left">申请信息</a-divider>
          <a-row :gutter="24">
            <a-col :span="12">
              <a-form-item label="申请ID">
                <copy-text :text="requestDetail.requestId" />
              </a-form-item>
            </a-col>
            <a-col :span="12">
              <a-form-item label="申请时间">
                {{ requestDetail.requestTime }}
              </a-form-item>
            </a-col>
          </a-row>
          <a-row :gutter="24">
            <a-col :span="12">
              <a-form-item label="状态">
                <a-tag :color="STATUS_COLOR_MAP[requestDetail.status]">
                  {{ STATUS_TEXT_MAP[requestDetail.status] }}
                </a-tag>
              </a-form-item>
            </a-col>
            <a-col :span="12">
              <a-form-item label="申请理由">
                {{ requestDetail.reason || '-' }}
              </a-form-item>
            </a-col>
          </a-row>

          <!-- 权限列表 -->
          <a-divider orientation="left">
            申请权限
            <a-checkbox
              v-if="hasPendingItems"
              style="margin-left: 8px"
              :indeterminate="indeterminate"
              :checked="allSelected"
              @change="handleSelectAllChange"
            >
              全选
            </a-checkbox>
          </a-divider>

          <!-- 批量操作按钮 -->
          <div v-if="hasPendingItems" style="margin-bottom: 12px">
            <a-space>
              <a-button type="primary" :disabled="selectedItemIds.length === 0" @click="handleBatchApprove">
                批准选中 ({{ selectedItemIds.length }})
              </a-button>
              <a-button type="primary" status="danger" :disabled="selectedItemIds.length === 0" @click="handleBatchReject">
                拒绝选中 ({{ selectedItemIds.length }})
              </a-button>
            </a-space>
          </div>

          <a-table
            :data="requestDetail.items"
            :bordered="false"
            :pagination="false"
            size="small"
          >
            <template #columns>
              <a-table-column :width="40" v-if="hasPendingItems">
                <template #cell="{ record }">
                  <a-checkbox
                    v-if="record.status === 'PENDING'"
                    :model-value="selectedItemIds.includes(record.itemId)"
                    @change="handleItemCheck(record.itemId)"
                  />
                </template>
              </a-table-column>
              <a-table-column title="权限名称" data-index="permissionName" />
              <a-table-column title="权限标识" data-index="permissionCode" :width="200" />
              <a-table-column title="状态" :width="100" align="center">
                <template #cell="{ record }">
                  <a-tag :color="STATUS_COLOR_MAP[record.status]">
                    {{ STATUS_TEXT_MAP[record.status] }}
                  </a-tag>
                </template>
              </a-table-column>
              <a-table-column title="拒绝理由" :width="150">
                <template #cell="{ record }">
                  {{ record.rejectReason || '-' }}
                </template>
              </a-table-column>
              <a-table-column title="操作" :width="150" align="center" v-if="hasPendingItems">
                <template #cell="{ record }">
                  <template v-if="record.status === 'PENDING'">
                    <a-button type="text" size="small" @click="handleSingleApprove(record.itemId)">
                      批准
                    </a-button>
                    <a-button type="text" size="small" status="danger" @click="handleSingleReject(record.itemId)">
                      拒绝
                    </a-button>
                  </template>
                  <span v-else style="color: #999">-</span>
                </template>
              </a-table-column>
            </template>
          </a-table>

          <!-- 审批结果 -->
          <template v-if="requestDetail.approvalInfo">
            <a-divider orientation="left">审批结果</a-divider>
            <a-row :gutter="24">
              <a-col :span="12">
                <a-form-item label="审批时间">
                  {{ requestDetail.approvalInfo.approvedAt }}
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item label="审批人">
                  {{ requestDetail.approvalInfo.approvedBy }}
                </a-form-item>
              </a-col>
            </a-row>
          </template>
        </a-form>
      </a-spin>
    </a-modal>

    <!-- 批准弹窗 -->
    <a-modal
      v-model:visible="approveModalVisible"
      title="批准权限申请"
      :width="500"
      @before-ok="handleApproveSubmit"
      @cancel="handleApproveCancel"
    >
      <a-form :model="approveForm" layout="vertical">
        <a-form-item label="将批准的权限">
          <a-text>{{ approveForm.itemIds.length }} 个权限</a-text>
        </a-form-item>

        <a-form-item label="限制条件">
          <a-select
            v-model:model-value="approveForm.expressionIds"
            placeholder="请选择限制条件（可选）"
            multiple
            allow-clear
            allow-search
          >
            <a-option
              v-for="exp in expressionList"
              :key="exp.id"
              :value="exp.id"
            >
              {{ exp.name }}
            </a-option>
          </a-select>
        </a-form-item>

        <a-form-item label="优先级">
          <a-select
            v-model:model-value="approveForm.priority"
            placeholder="请选择优先级（可选）"
            allow-clear
          >
            <a-option :value="-1">最低</a-option>
            <a-option :value="0">低</a-option>
            <a-option :value="1">中</a-option>
            <a-option :value="2">高</a-option>
            <a-option :value="3">最高</a-option>
          </a-select>
        </a-form-item>
      </a-form>
    </a-modal>

    <!-- 拒绝弹窗 -->
    <a-modal
      v-model:visible="rejectModalVisible"
      title="拒绝权限申请"
      :width="500"
      @before-ok="handleRejectSubmit"
      @cancel="handleRejectCancel"
    >
      <a-form :model="rejectForm" layout="vertical">
        <a-form-item label="将拒绝的权限">
          <a-text>{{ rejectForm.itemIds.length }} 个权限</a-text>
        </a-form-item>

        <a-form-item
          label="拒绝理由"
          required
          :validate-status="rejectForm.rejectReason.length > 200 ? 'error' : ''"
          :help="rejectForm.rejectReason.length > 200 ? '拒绝理由不能超过200字' : ''"
        >
          <a-textarea
            v-model:model-value="rejectForm.rejectReason"
            placeholder="请输入拒绝理由（必填）"
            :maxlength="200"
            show-word-limit
            :rows="3"
          />
        </a-form-item>
      </a-form>
    </a-modal>
  </div>
</template>
