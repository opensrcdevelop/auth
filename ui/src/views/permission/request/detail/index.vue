<script lang="ts">
import detailTs from "./index";

export default detailTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="detail-header">
        <div>
          <span class="title">权限申请详情</span>
          <div class="id">
            <span>ID:&nbsp;&nbsp;</span>
            <copy-text :text="requestId" textColor="#86909c" />
          </div>
        </div>
        <a-space>
          <a-button
            type="primary"
            :disabled="!requestInfo.pendingCount || requestInfo.pendingCount === 0"
            @click="handleOpenApproveModal"
          >
            批准
          </a-button>
          <a-button
            type="primary"
            status="danger"
            :disabled="!requestInfo.pendingCount || requestInfo.pendingCount === 0"
            @click="handleOpenRejectModal"
          >
            拒绝
          </a-button>
        </a-space>
      </div>

      <a-tabs>
        <a-tab-pane key="request_info" title="申请信息">
          <div class="tab-container">
            <div class="info-title">申请人信息</div>
            <a-descriptions :column="1">
              <a-descriptions-item label="申请人">
                <span
                  class="table-column-name"
                  @click="handleToUserDetail(requestInfo.userId)"
                >
                  {{ requestInfo.username || requestInfo.userId }}
                </span>
              </a-descriptions-item>
              <a-descriptions-item label="申请时间">
                {{ requestInfo.requestTime || "-" }}
              </a-descriptions-item>
              <a-descriptions-item label="申请理由">
                {{ requestInfo.reason || "-" }}
              </a-descriptions-item>
            </a-descriptions>

            <div class="info-title" style="margin-top: 24px">审批统计</div>
            <a-descriptions :column="5">
              <a-descriptions-item label="待审批">
                <a-tag color="arcoblue">{{ requestInfo.pendingCount }}</a-tag>
              </a-descriptions-item>
              <a-descriptions-item label="已批准">
                <a-tag color="green">{{ requestInfo.approvedCount }}</a-tag>
              </a-descriptions-item>
              <a-descriptions-item label="自动批准">
                <a-tag color="cyan">{{ requestInfo.autoApproveCount }}</a-tag>
              </a-descriptions-item>
              <a-descriptions-item label="已拒绝">
                <a-tag color="red">{{ requestInfo.rejectedCount }}</a-tag>
              </a-descriptions-item>
              <a-descriptions-item label="总计">
                <a-tag>{{ requestInfo.totalCount }}</a-tag>
              </a-descriptions-item>
            </a-descriptions>
          </div>
        </a-tab-pane>

        <a-tab-pane key="permission_items" title="权限明细">
          <div class="tab-container">
            <div class="info-title">权限明细</div>
            <div class="table-toolbar">
              <a-space wrap>
                <a-tag
                  v-for="status in [
                    'PENDING',
                    'APPROVED',
                    'AUTO_APPROVED',
                    'REJECTED',
                  ]"
                  :key="status"
                  :color="statusFilter.includes(status) ? 'arcoblue' : 'gray'"
                  checkable
                  @click="
                    () => {
                      if (statusFilter.includes(status)) {
                        statusFilter = statusFilter.filter((s) => s !== status);
                      } else {
                        statusFilter = [...statusFilter, status];
                      }
                    }
                  "
                >
                  {{ getStatusText(status) }}
                </a-tag>
              </a-space>
              <div class="selected-info" v-if="selectedItems.length > 0">
                已选择 {{ selectedItems.length }} 项
                <a-button
                  type="text"
                  size="small"
                  @click="handleClearSelection"
                >
                  清空
                </a-button>
              </div>
            </div>

            <a-table
              :data="filteredItems"
              :bordered="false"
              :row-key="'id'"
              :pagination="{
                showTotal: true,
                showPageSize: true,
                defaultPageSize: 15,
                pageSizeOptions: [15, 50, 100],
              }"
            >
              <template #columns>
                <a-table-column :width="50" align="center">
                  <template #title>
                    <a-checkbox
                      :disabled="!hasPendingItems"
                      v-model="isAllPendingSelected"
                      :indeterminate="isSomePendingSelected"
                      @change="handleToggleSelectAllPending"
                    />
                  </template>
                  <template #cell="{ record }">
                    <a-checkbox
                      :disabled="record.status !== 'PENDING'"
                      :value="record.id"
                      v-model="selectedItems"
                    />
                  </template>
                </a-table-column>
                <a-table-column title="资源组" ellipsis tooltip>
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="
                        handleToResourceGroupDetail(record.resourceGroupId)
                      "
                    >
                      {{ record.resourceGroupName }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column title="资源" ellipsis tooltip>
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handleToResourceDetail(record.resourceId)"
                    >
                      {{ record.resourceName }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column title="权限" ellipsis tooltip>
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handleToPermissionDetail(record.permissionId)"
                    >
                      {{ record.permissionName }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column title="状态" :width="100">
                  <template #cell="{ record }">
                    <a-tag :color="getStatusColor(record.status)">
                      {{ getStatusText(record.status) }}
                    </a-tag>
                  </template>
                </a-table-column>
                <a-table-column title="审批人" :width="120">
                  <template #cell="{ record }">
                    <span
                      v-if="record.approverUsername"
                      class="table-column-name"
                      @click="handleToUserDetail(record.approverId)"
                    >
                      {{ record.approverUsername }}
                    </span>
                    <span v-else>-</span>
                  </template>
                </a-table-column>
                <a-table-column title="审批时间" ellipsis tooltip>
                  <template #cell="{ record }">
                    {{ record.approveTime || "-" }}
                  </template>
                </a-table-column>
                <a-table-column title="拒绝理由" ellipsis tooltip>
                  <template #cell="{ record }">
                    {{ record.rejectReason || "-" }}
                  </template>
                </a-table-column>
              </template>
            </a-table>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>

    <!-- 批准/拒绝模态框 -->
    <a-modal
      :visible="approveModalVisible"
      :title="approveModalMode === 'approve' ? '批准权限申请' : '拒绝权限申请'"
      @cancel="handleCloseApproveModal"
      :footer="false"
      width="600px"
    >
      <a-form
        :model="approveForm"
        ref="approveFormRef"
        :rules="approveFormRules"
        layout="vertical"
        @submit="handleApproveFormSubmit"
      >
        <!-- 批准模式：选择限制条件 -->
        <a-form-item
          v-if="approveModalMode === 'approve'"
          field="expressionIds"
          label="限制条件（可选）"
        >
          <a-select
            v-model="approveForm.expressionIds"
            v-model:input-value="expressionSearchKeyword"
            multiple
            allow-clear
            allow-search
            placeholder="请选择限制条件（可选）"
            :filter-option="false"
            @search="handleSearchExpression"
            @dropdown-reach-bottom="handleLoadMoreExpression"
          >
            <a-option
              v-for="item in expressionList"
              :key="item.id"
              :value="item.id"
            >
              {{ item.name }}
            </a-option>
          </a-select>
          <template #extra>
            <div style="color: #86909c; font-size: 12px">
              选择批准时应用的限制条件。多个条件会同时生效。
            </div>
          </template>
        </a-form-item>

        <!-- 拒绝模式：输入拒绝理由 -->
        <a-form-item
          v-if="approveModalMode === 'reject'"
          field="rejectReason"
          label="拒绝理由（可选）"
        >
          <a-textarea
            v-model="approveForm.rejectReason"
            placeholder="请输入拒绝理由（可选）"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>

        <a-form-item>
          <div class="form-footer">
            <a-space>
              <a-button @click="handleCloseApproveModal">取消</a-button>
              <a-button
                type="primary"
                html-type="submit"
                :loading="approveFormSubmitLoading"
                :status="approveModalMode === 'approve' ? 'normal' : 'danger'"
              >
                {{ approveModalMode === "approve" ? "批准" : "拒绝" }}
              </a-button>
            </a-space>
          </div>
        </a-form-item>
      </a-form>
    </a-modal>
  </div>
</template>
