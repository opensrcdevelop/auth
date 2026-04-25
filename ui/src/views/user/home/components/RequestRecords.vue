<template>
  <div class="request-records">
    <a-table
      :data="records"
      :bordered="false"
      :pagination="requestRecordsPagination.pagination"
      @page-change="requestRecordsPagination.handlePageChange"
      @page-size-change="requestRecordsPagination.handlePageSizeChange"
    >
      <template #columns>
        <a-table-column
          title="申请时间"
          data-index="requestTime"
          :width="180"
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        >
          <template #cell="{ record }">
            {{ record.requestTime }}
          </template>
        </a-table-column>
        <a-table-column title="申请理由" ellipsis tooltip data-index="reason" />
        <a-table-column title="待审批" :width="100">
          <template #cell="{ record }">
            <a-tag v-if="record.pendingCount > 0" color="arcoblue">
              {{ record.pendingCount }}
            </a-tag>
            <span v-else class="zero-count">0</span>
          </template>
        </a-table-column>
        <a-table-column title="已批准" :width="80">
          <template #cell="{ record }">
            <a-tag v-if="record.approvedCount > 0" color="green">
              {{ record.approvedCount }}
            </a-tag>
            <span v-else class="zero-count">0</span>
          </template>
        </a-table-column>
        <a-table-column title="自动批准" :width="100">
          <template #cell="{ record }">
            <a-tag v-if="record.autoApproveCount > 0" color="cyan">
              {{ record.autoApproveCount }}
            </a-tag>
            <span v-else class="zero-count">0</span>
          </template>
        </a-table-column>
        <a-table-column title="已拒绝" :width="80">
          <template #cell="{ record }">
            <a-tag v-if="record.rejectedCount > 0" color="red">
              {{ record.rejectedCount }}
            </a-tag>
            <span v-else class="zero-count">0</span>
          </template>
        </a-table-column>
        <a-table-column title="总计" :width="80">
          <template #cell="{ record }">
            {{ record.totalCount }}
          </template>
        </a-table-column>
        <a-table-column title="操作" :width="120" :fixed="'right'">
          <template #cell="{ record }">
            <a-space>
              <a-button size="mini" @click="handleViewItems(record)">
                <template #icon>
                  <a-tooltip content="查看明细">
                    <icon-eye />
                  </a-tooltip>
                </template>
              </a-button>
              <a-button
                size="mini"
                status="danger"
                @click="handleCancelRequest(record)"
              >
                <template #icon>
                  <a-tooltip content="取消申请">
                    <icon-undo />
                  </a-tooltip>
                </template>
              </a-button>
            </a-space>
          </template>
        </a-table-column>
      </template>
    </a-table>

    <!-- 明细 -->
    <a-drawer
      v-model:visible="itemsDrawerVisible"
      title="权限申请明细"
      width="60%"
      :footer="false"
    >
      <a-table :data="items" :bordered="false" :pagination="false">
        <template #columns>
          <a-table-column title="权限" ellipsis tooltip>
            <template #cell="{ record }">
              <span class="permission-text">
                {{ record.resourceGroupName }} / {{ record.resourceName }} /
                {{ record.permissionName }}</span
              >
            </template>
          </a-table-column>
          <a-table-column title="状态" :width="80">
            <template #cell="{ record }">
              <a-tag :color="getStatusColor(record.status)">
                {{ getStatusText(record.status) }}
              </a-tag>
            </template>
          </a-table-column>
          <a-table-column
            title="审批人"
            data-index="approverUsername"
            :width="100"
          >
            <template #cell="{ record }">
              {{ record.approverUsername || "-" }}
            </template>
          </a-table-column>
          <a-table-column title="拒绝理由" ellipsis tooltip>
            <template #cell="{ record }">
              {{ record.rejectReason || "-" }}
            </template>
          </a-table-column>
        </template>
      </a-table>
    </a-drawer>
  </div>
</template>

<script setup lang="ts">
import {ref, watch} from "vue";
import {Modal, Notification} from "@arco-design/web-vue";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {
  cancelMyPermissionRequest,
  getCurrentUserPermissionRequestItems,
  getMyPermissionRequests,
} from "@/api/permission";
import {usePagination} from "@/hooks/usePagination";

interface PermissionRequestRecord {
  requestId: string;
  userId: string;
  reason: string;
  requestTime: string;
  pendingCount: number;
  approvedCount: number;
  autoApproveCount: number;
  rejectedCount: number;
  totalCount: number;
}

interface PermissionRequestItem {
  permissionId: string;
  permissionName: string;
  resourceId: string;
  resourceName: string;
  resourceGroupId: string;
  resourceGroupName: string;
  status: string;
  rejectReason: string;
  approverUsername: string;
}

const props = withDefaults(
  defineProps<{
    userInfo: any;
    activeKey?: string;
  }>(),
  {
    userInfo: () => ({}),
    activeKey: "",
  },
);

const records = ref<PermissionRequestRecord[]>([]);
const requestRecordsPagination = usePagination(
  `${props.userInfo.userId}_requestRecords`,
  ({ page, size }: { page: number; size: number }) => {
    handleLoadRecords(page, size);
  },
);

// 明细状态
const itemsDrawerVisible = ref(false);
const items = ref<PermissionRequestItem[]>([]);

// 状态颜色映射
const statusColorMap: Record<string, string> = {
  PENDING: "arcoblue",
  APPROVED: "green",
  AUTO_APPROVED: "cyan",
  REJECTED: "red",
};

// 状态文本映射
const statusTextMap: Record<string, string> = {
  PENDING: "待审批",
  APPROVED: "已批准",
  AUTO_APPROVED: "自动批准",
  REJECTED: "已拒绝",
};

const getStatusColor = (status: string) => statusColorMap[status] || "gray";
const getStatusText = (status: string) => statusTextMap[status] || status;

// 加载权限申请记录
const handleLoadRecords = (page: number = 1, size: number = 15) => {
  getMyPermissionRequests({
    page,
    size,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        records.value = data?.list || [];
        requestRecordsPagination.updatePagination(
          data.current,
          data.total,
          data.size,
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限申请记录");
    });
};

// 查看明细
const handleViewItems = (record: PermissionRequestRecord) => {
  getCurrentUserPermissionRequestItems(record.requestId)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        itemsDrawerVisible.value = true;
        items.value = data || [];
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限申请明细");
    });
};

// 取消申请
const handleCancelRequest = (record: PermissionRequestRecord) => {
  Modal.confirm({
    title: "确定取消该权限申请吗？",
    content: "自动批准的权限将会取消授权。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      cancelMyPermissionRequest(record.requestId)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("取消权限申请成功");
            handleLoadRecords();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "取消权限申请");
        });
    },
  });
};

// 执行初始化
const handleInit = () => {
  handleLoadRecords();
};

// 监听 tab 切换
watch(
  () => props.activeKey,
  (newActiveKey) => {
    if (newActiveKey === "request_records") {
      handleInit();
    }
  },
  { immediate: true },
);
</script>

<style lang="scss" scoped>
.request-records {
  .zero-count {
    color: #86909c;
  }
}

.permission-text {
  font-size: 12px;
  font-weight: 500;
  padding: 4px 8px;
  border-radius: 2px;
  background-color: var(--color-neutral-3);
}
</style>
