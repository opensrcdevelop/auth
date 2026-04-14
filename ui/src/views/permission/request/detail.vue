<script setup lang="ts">
import { onMounted } from 'vue';
import { IconLeft } from '@arco-design/web-vue/es/icon';
import detailTs from './detail';
import {
  requestId,
  loading,
  detail,
  selectedItemIds,
  approveModalVisible,
  approveLoading,
  approveForm,
  rejectModalVisible,
  rejectLoading,
  rejectForm,
  expressionList,
  hasPendingItems,
  allSelected,
  indeterminate,
  STATUS_COLOR_MAP,
  STATUS_TEXT_MAP,
  loadDetail,
  handleItemCheck,
  handleSelectAllChange,
  handleBatchApprove,
  handleSingleApprove,
  handleApproveSubmit,
  handleApproveCancel,
  handleBatchReject,
  handleSingleReject,
  handleRejectSubmit,
  handleRejectCancel,
} from './detail';

onMounted(() => {
  loadDetail();
});
</script>

<template>
  <div class="request-detail">
    <!-- 返回按钮和标题 -->
    <div class="detail-header">
      <a-button @click="$router.back()">
        <template #icon><IconLeft /></template>
        返回
      </a-button>
      <div class="title">申请详情</div>
    </div>

    <a-spin :loading="loading" style="width: 100%">
      <a-form v-if="detail" layout="vertical">
        <!-- 申请信息 -->
        <a-divider orientation="left">申请信息</a-divider>
        <a-row :gutter="24">
          <a-col :span="8">
            <a-form-item label="申请ID">
              <copy-text :text="detail.requestId" />
            </a-form-item>
          </a-col>
          <a-col :span="8">
            <a-form-item label="申请时间">
              {{ detail.requestTime }}
            </a-form-item>
          </a-col>
          <a-col :span="8">
            <a-form-item label="状态">
              <a-tag :color="STATUS_COLOR_MAP[detail.status]">
                {{ STATUS_TEXT_MAP[detail.status] }}
              </a-tag>
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item label="申请理由">
          {{ detail.reason || '-' }}
        </a-form-item>

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
            <a-button
              type="primary"
              :disabled="selectedItemIds.length === 0"
              @click="handleBatchApprove"
            >
              批准选中 ({{ selectedItemIds.length }})
            </a-button>
            <a-button
              type="primary"
              status="danger"
              :disabled="selectedItemIds.length === 0"
              @click="handleBatchReject"
            >
              拒绝选中 ({{ selectedItemIds.length }})
            </a-button>
          </a-space>
        </div>

        <!-- 权限项表格 -->
        <a-table :data="detail.items" :pagination="false" size="small">
          <template #columns>
            <a-table-column :width="40" v-if="hasPendingItems">
              <template #cell="{ record }">
                <a-checkbox
                  v-if="record.status === 'PENDING'"
                  :value="record.itemId"
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
        <template v-if="detail.approvalInfo">
          <a-divider orientation="left">审批结果</a-divider>
          <a-row :gutter="24">
            <a-col :span="12">
              <a-form-item label="审批时间">
                {{ detail.approvalInfo.approvedAt }}
              </a-form-item>
            </a-col>
            <a-col :span="12">
              <a-form-item label="审批人">
                {{ detail.approvalInfo.approvedBy }}
              </a-form-item>
            </a-col>
          </a-row>
        </template>
      </a-form>
    </a-spin>

    <!-- 批准弹窗 -->
    <a-modal
      v-model:visible="approveModalVisible"
      title="批准权限申请"
      :width="500"
      :loading="approveLoading"
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
            <a-option v-for="exp in expressionList" :key="exp.id" :value="exp.id">
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
      :loading="rejectLoading"
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

<style lang="scss" scoped>
.request-detail {
  width: 100%;
  padding: 16px;
}

.detail-header {
  display: flex;
  align-items: center;
  gap: 16px;
  margin-bottom: 24px;

  .title {
    font-size: 16px;
    font-weight: 500;
    color: var(--color-text-1);
  }
}
</style>
