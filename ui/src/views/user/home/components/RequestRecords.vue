<script lang="ts">
import {defineComponent, onMounted, ref, reactive} from 'vue';
import {getRequestList, getRequestDetail} from '@/api/userPermissionRequest';
import {handleApiSuccess, handleApiError} from '@/util/tool';

export default defineComponent({
  name: 'RequestRecords',
  setup() {
    const loading = ref(false);
    const requestList = ref([]);
    const pagination = reactive({
      current: 1,
      pageSize: 10,
      total: 0,
    });

    // 详情弹窗
    const detailModalVisible = ref(false);
    const detailLoading = ref(false);
    const requestDetail = ref<any>(null);

    // 状态映射（D-14）
    const STATUS_TEXT_MAP: Record<string, string> = {
      PENDING: '待审批',
      APPROVED: '已批准',
      REJECTED: '已拒绝',
      AUTO_APPROVED: '自动批准',
    };

    const STATUS_COLOR_MAP: Record<string, string> = {
      PENDING: 'arcoblue',
      APPROVED: 'green',
      REJECTED: 'red',
      AUTO_APPROVED: 'purple',
    };

    const renderStatus = (status: string) => {
      return STATUS_TEXT_MAP[status] || status;
    };

    const getStatusColor = (status: string) => {
      return STATUS_COLOR_MAP[status] || 'gray';
    };

    // 加载申请记录列表
    const handleLoadRequestList = () => {
      loading.value = true;
      getRequestList({
        page: pagination.current,
        size: pagination.pageSize,
      })
          .then((result: any) => {
            handleApiSuccess(result, (data: any) => {
              requestList.value = data?.records || [];
              pagination.total = data?.total || 0;
            });
          })
          .catch((err: any) => {
            handleApiError(err, '获取申请记录');
          })
          .finally(() => {
            loading.value = false;
          });
    };

    // 分页变化
    const handlePageChange = (page: number) => {
      pagination.current = page;
      handleLoadRequestList();
    };

    const handlePageSizeChange = (pageSize: number) => {
      pagination.pageSize = pageSize;
      pagination.current = 1;
      handleLoadRequestList();
    };

    // 查看详情
    const handleViewDetail = (requestId: string) => {
      detailModalVisible.value = true;
      detailLoading.value = true;
      getRequestDetail(requestId)
          .then((result: any) => {
            handleApiSuccess(result, (data: any) => {
              requestDetail.value = data;
            });
          })
          .catch((err: any) => {
            handleApiError(err, '获取申请详情');
          })
          .finally(() => {
            detailLoading.value = false;
          });
    };

    // 关闭详情弹窗
    const handleCloseDetail = () => {
      detailModalVisible.value = false;
      requestDetail.value = null;
    };

    onMounted(() => {
      handleLoadRequestList();
    });

    return {
      loading,
      requestList,
      pagination,
      handlePageChange,
      handlePageSizeChange,
      handleViewDetail,
      detailModalVisible,
      detailLoading,
      requestDetail,
      handleCloseDetail,
      renderStatus,
      getStatusColor,
      STATUS_TEXT_MAP,
    };
  },
});
</script>

<template>
  <a-spin :loading="loading" style="width: 100%">
    <div class="request-records-container">
      <!-- 申请记录列表 -->
      <a-table
          :data="requestList"
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
          <a-table-column title="申请ID" data-index="requestId" :width="200">
            <template #cell="{ record }">
              <copy-text :text="record.requestId" />
            </template>
          </a-table-column>
          <a-table-column title="申请时间" data-index="requestTime" :width="180" />
          <a-table-column title="状态" :width="100" align="center">
            <template #cell="{ record }">
              <a-tag :color="getStatusColor(record.status)">
                {{ renderStatus(record.status) }}
              </a-tag>
            </template>
          </a-table-column>
          <a-table-column title="权限数量" :width="100" align="center">
            <template #cell="{ record }">
              {{ record.itemCount }}
            </template>
          </a-table-column>
          <a-table-column title="操作" :width="100" align="center">
            <template #cell="{ record }">
              <a-button type="text" size="small" @click="handleViewDetail(record.requestId)">
                查看详情
              </a-button>
            </template>
          </a-table-column>
        </template>
      </a-table>

      <!-- 详情弹窗 -->
      <a-modal
          v-model:visible="detailModalVisible"
          title="申请详情"
          :width="600"
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
                  <a-tag :color="getStatusColor(requestDetail.status)">
                    {{ renderStatus(requestDetail.status) }}
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
            <a-divider orientation="left">申请权限</a-divider>
            <a-table
                :data="requestDetail.items"
                :bordered="false"
                :pagination="false"
                size="small"
            >
              <template #columns>
                <a-table-column title="权限名称" data-index="permissionName" />
                <a-table-column title="权限标识" data-index="permissionCode" :width="200" />
                <a-table-column title="状态" :width="100" align="center">
                  <template #cell="{ record }">
                    <a-tag :color="getStatusColor(record.status)">
                      {{ renderStatus(record.status) }}
                    </a-tag>
                  </template>
                </a-table-column>
                <a-table-column title="拒绝理由" :width="150">
                  <template #cell="{ record }">
                    {{ record.rejectReason || '-' }}
                  </template>
                </a-table-column>
              </template>
            </a-table>

            <!-- 审批结果 -->
            <template v-if="requestDetail.status !== 'PENDING' && requestDetail.status !== 'AUTO_APPROVED'">
              <a-divider orientation="left">审批结果</a-divider>
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item label="审批时间">
                    {{ requestDetail.approvalInfo?.approvedAt || '-' }}
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item label="审批人">
                    {{ requestDetail.approvalInfo?.approvedBy || '-' }}
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item v-if="requestDetail.approvalInfo?.rejectReason" label="拒绝理由">
                {{ requestDetail.approvalInfo.rejectReason }}
              </a-form-item>
            </template>
          </a-form>
        </a-spin>
      </a-modal>
    </div>
  </a-spin>
</template>

<style lang="scss" scoped>
.request-records-container {
  width: 100%;
}
</style>
