import { defineComponent, ref, reactive, computed, onMounted } from 'vue';
import { Message } from '@arco-design/web-vue';
import { getPendingList, getAllList, approveRequest, rejectRequest, getRequestDetail } from '@/api/adminPermissionRequest';
import { getPermissionExpList } from '@/api/permission';
import { handleApiError, handleApiSuccess } from '@/util/tool';

export default defineComponent({
  name: 'PermissionRequestApproval',
  setup() {
    const loading = ref(false);
    const activeTab = ref('pending');

    // 待审批列表
    const pendingList = ref<any[]>([]);

    // 全部申请列表
    const allList = ref<any[]>([]);

    // 分页配置（D-08）
    const pagination = reactive({
      current: 1,
      pageSize: 15,
      total: 0,
    });

    // 状态选项（D-04）
    const statusOptions = [
      { label: '待审批', value: 'PENDING' },
      { label: '已批准', value: 'APPROVED' },
      { label: '已拒绝', value: 'REJECTED' },
      { label: '自动批准', value: 'AUTO_APPROVED' },
    ];

    // 多选状态筛选（D-05）
    const selectedStatuses = ref<string[]>([]);

    // 状态颜色映射（D-12）
    const STATUS_COLOR_MAP: Record<string, string> = {
      PENDING: 'arcoblue',
      APPROVED: 'green',
      REJECTED: 'red',
      AUTO_APPROVED: 'purple',
    };

    // 状态文本映射（D-12）
    const STATUS_TEXT_MAP: Record<string, string> = {
      PENDING: '待审批',
      APPROVED: '已批准',
      REJECTED: '已拒绝',
      AUTO_APPROVED: '自动批准',
    };

    // 表格列定义（D-07）
    const columns = [
      { title: '申请ID', dataIndex: 'requestId', width: 200 },
      { title: '申请时间', dataIndex: 'requestTime', width: 180 },
      { title: '申请理由', dataIndex: 'reason', ellipsis: true },
      { title: '权限数量', dataIndex: 'itemCount', width: 100, align: 'center' as const },
      { title: '状态', dataIndex: 'status', width: 100, align: 'center' as const },
      { title: '操作', width: 100, align: 'center' as const },
    ];

    // 渲染状态文本
    const renderStatus = (status: string) => {
      return STATUS_TEXT_MAP[status] || status;
    };

    // 获取状态颜色
    const getStatusColor = (status: string) => {
      return STATUS_COLOR_MAP[status] || 'gray';
    };

    // Tab 切换
    const handleTabChange = (tab: string) => {
      activeTab.value = tab;
      pagination.current = 1;
      selectedStatuses.value = [];
      if (tab === 'pending') {
        handleLoadPendingList();
      } else {
        handleLoadAllList();
      }
    };

    // 加载待审批列表
    const handleLoadPendingList = () => {
      loading.value = true;
      getPendingList({
        page: pagination.current,
        size: pagination.pageSize,
      })
        .then((result: any) => {
          handleApiSuccess(result, (data: any) => {
            pendingList.value = data?.records || [];
            pagination.total = data?.total || 0;
          });
        })
        .catch((err: any) => {
          handleApiError(err, '获取待审批列表');
        })
        .finally(() => {
          loading.value = false;
        });
    };

    // 加载全部申请列表（D-05, D-06）
    const handleLoadAllList = () => {
      loading.value = true;
      const params: any = {
        page: pagination.current,
        size: pagination.pageSize,
      };
      if (selectedStatuses.value.length > 0) {
        // 多选状态筛选，传第一个（API设计为单状态）
        params.status = selectedStatuses.value[0];
      }
      getAllList(params)
        .then((result: any) => {
          handleApiSuccess(result, (data: any) => {
            allList.value = data?.records || [];
            pagination.total = data?.total || 0;
          });
        })
        .catch((err: any) => {
          handleApiError(err, '获取全部申请列表');
        })
        .finally(() => {
          loading.value = false;
        });
    };

    // 分页变化
    const handlePageChange = (page: number) => {
      pagination.current = page;
      if (activeTab.value === 'pending') {
        handleLoadPendingList();
      } else {
        handleLoadAllList();
      }
    };

    // 每页条数变化
    const handlePageSizeChange = (pageSize: number) => {
      pagination.pageSize = pageSize;
      pagination.current = 1;
      if (activeTab.value === 'pending') {
        handleLoadPendingList();
      } else {
        handleLoadAllList();
      }
    };

    // 状态筛选变化（D-05）
    const handleStatusFilterChange = () => {
      pagination.current = 1;
      handleLoadAllList();
    };

    // 详情弹窗
    const detailModalVisible = ref(false);
    const detailLoading = ref(false);
    const requestDetail = ref<any>(null);

    // 选中项（用于批量操作）
    const selectedItemIds = ref<string[]>([]);

    // 批准弹窗
    const approveModalVisible = ref(false);
    const approveLoading = ref(false);
    const approveForm = reactive({
      itemIds: [] as string[],
      expressionIds: [] as string[],
      priority: undefined as number | undefined,
    });

    // 拒绝弹窗
    const rejectModalVisible = ref(false);
    const rejectLoading = ref(false);
    const rejectForm = reactive({
      itemIds: [] as string[],
      rejectReason: '',
    });

    // 限制条件列表
    const expressionList = ref<any[]>([]);

    // 是否有待审批的子项
    const hasPendingItems = computed(() => {
      return requestDetail.value?.items?.some((item: any) => item.status === 'PENDING') ?? false;
    });

    // 全选状态
    const allSelected = computed(() => {
      const pendingItems = requestDetail.value?.items?.filter((item: any) => item.status === 'PENDING') ?? [];
      return pendingItems.length > 0 && selectedItemIds.value.length === pendingItems.length;
    });

    // 不确定状态
    const indeterminate = computed(() => {
      const pendingItems = requestDetail.value?.items?.filter((item: any) => item.status === 'PENDING') ?? [];
      return selectedItemIds.value.length > 0 && selectedItemIds.value.length < pendingItems.length;
    });

    // 查看详情
    const handleViewDetail = (requestId: string) => {
      detailModalVisible.value = true;
      detailLoading.value = true;
      selectedItemIds.value = [];
      getRequestDetail(requestId)
        .then((result: any) => {
          handleApiSuccess(result, (data: any) => {
            requestDetail.value = data;
          });
        })
        .catch((err: any) => {
          handleApiError(err, '获取申请详情');
          detailModalVisible.value = false;
        })
        .finally(() => {
          detailLoading.value = false;
        });
    };

    // 关闭详情
    const handleCloseDetail = () => {
      detailModalVisible.value = false;
      requestDetail.value = null;
      selectedItemIds.value = [];
    };

    // 选择变化
    const handleSelectionChange = (keys: string[]) => {
      selectedItemIds.value = keys;
    };

    // 全选变化
    const handleSelectAllChange = (checked: boolean) => {
      if (checked) {
        selectedItemIds.value = requestDetail.value?.items
          ?.filter((item: any) => item.status === 'PENDING')
          ?.map((item: any) => item.itemId) ?? [];
      } else {
        selectedItemIds.value = [];
      }
    };

    // 单个勾选
    const handleItemCheck = (itemId: string) => {
      const index = selectedItemIds.value.indexOf(itemId);
      if (index > -1) {
        selectedItemIds.value.splice(index, 1);
      } else {
        selectedItemIds.value.push(itemId);
      }
    };

    // 加载限制条件列表
    const loadExpressionList = () => {
      getPermissionExpList({})
        .then((result: any) => {
          handleApiSuccess(result, (data: any) => {
            expressionList.value = data || [];
          });
        })
        .catch((err: any) => {
          handleApiError(err, '获取限制条件列表');
        });
    };

    // 批量批准
    const handleBatchApprove = () => {
      approveForm.itemIds = [...selectedItemIds.value];
      approveForm.expressionIds = [];
      approveForm.priority = undefined;
      loadExpressionList();
      approveModalVisible.value = true;
    };

    // 单个批准
    const handleSingleApprove = (itemId: string) => {
      approveForm.itemIds = [itemId];
      approveForm.expressionIds = [];
      approveForm.priority = undefined;
      loadExpressionList();
      approveModalVisible.value = true;
    };

    // 批准提交
    const handleApproveSubmit = async (done: Function) => {
      if (approveForm.itemIds.length === 0) {
        Message.warning('请选择要批准的权限');
        done(false);
        return;
      }

      approveLoading.value = true;
      try {
        await approveRequest(requestDetail.value.requestId, {
          itemIds: approveForm.itemIds,
          expressionIds: approveForm.expressionIds.length > 0 ? approveForm.expressionIds : undefined,
          priority: approveForm.priority,
        });
        Message.success('批准成功');
        approveModalVisible.value = false;
        await handleRefreshListAndDetail();
        done(true);
      } catch (err: any) {
        Message.error(err.message || '批准失败');
        done(false);
      } finally {
        approveLoading.value = false;
      }
    };

    // 批准取消
    const handleApproveCancel = () => {
      approveModalVisible.value = false;
      approveForm.itemIds = [];
      approveForm.expressionIds = [];
      approveForm.priority = undefined;
    };

    // 批量拒绝
    const handleBatchReject = () => {
      rejectForm.itemIds = [...selectedItemIds.value];
      rejectForm.rejectReason = '';
      rejectModalVisible.value = true;
    };

    // 单个拒绝
    const handleSingleReject = (itemId: string) => {
      rejectForm.itemIds = [itemId];
      rejectForm.rejectReason = '';
      rejectModalVisible.value = true;
    };

    // 拒绝提交
    const handleRejectSubmit = async (done: Function) => {
      if (!rejectForm.rejectReason || rejectForm.rejectReason.trim() === '') {
        Message.warning('请输入拒绝理由');
        done(false);
        return;
      }

      if (rejectForm.rejectReason.length > 200) {
        Message.warning('拒绝理由不能超过200字');
        done(false);
        return;
      }

      if (rejectForm.itemIds.length === 0) {
        Message.warning('请选择要拒绝的权限');
        done(false);
        return;
      }

      rejectLoading.value = true;
      try {
        await rejectRequest(requestDetail.value.requestId, {
          itemIds: rejectForm.itemIds,
          rejectReason: rejectForm.rejectReason,
        });
        Message.success('拒绝成功');
        rejectModalVisible.value = false;
        await handleRefreshListAndDetail();
        done(true);
      } catch (err: any) {
        Message.error(err.message || '拒绝失败');
        done(false);
      } finally {
        rejectLoading.value = false;
      }
    };

    // 拒绝取消
    const handleRejectCancel = () => {
      rejectModalVisible.value = false;
      rejectForm.itemIds = [];
      rejectForm.rejectReason = '';
    };

    // 刷新列表和详情
    const handleRefreshListAndDetail = async () => {
      if (activeTab.value === 'pending') {
        handleLoadPendingList();
      } else {
        handleLoadAllList();
      }
      // 刷新详情
      if (requestDetail.value) {
        detailLoading.value = true;
        try {
          const result = await getRequestDetail(requestDetail.value.requestId);
          handleApiSuccess(result, (data: any) => {
            requestDetail.value = data;
            // 清空已处理的选中项
            const pendingItemIds = requestDetail.value?.items
              ?.filter((item: any) => item.status === 'PENDING')
              ?.map((item: any) => item.itemId) ?? [];
            selectedItemIds.value = selectedItemIds.value.filter(id => pendingItemIds.includes(id));
          });
        } catch (err: any) {
          // ignore
        } finally {
          detailLoading.value = false;
        }
      }
    };

    onMounted(() => {
      handleLoadPendingList();
    });

    return {
      loading,
      activeTab,
      pendingList,
      allList,
      pagination,
      selectedStatuses,
      statusOptions,
      columns,
      STATUS_COLOR_MAP,
      STATUS_TEXT_MAP,
      renderStatus,
      getStatusColor,
      handleTabChange,
      handleLoadPendingList,
      handleLoadAllList,
      handlePageChange,
      handlePageSizeChange,
      handleStatusFilterChange,
      handleViewDetail,
      // 详情弹窗
      detailModalVisible,
      detailLoading,
      requestDetail,
      handleCloseDetail,
      // 选中项
      selectedItemIds,
      handleSelectionChange,
      handleSelectAllChange,
      handleItemCheck,
      hasPendingItems,
      allSelected,
      indeterminate,
      // 批准弹窗
      approveModalVisible,
      approveLoading,
      approveForm,
      expressionList,
      handleBatchApprove,
      handleSingleApprove,
      handleApproveSubmit,
      handleApproveCancel,
      // 拒绝弹窗
      rejectModalVisible,
      rejectLoading,
      rejectForm,
      handleBatchReject,
      handleSingleReject,
      handleRejectSubmit,
      handleRejectCancel,
    };
  },
});
