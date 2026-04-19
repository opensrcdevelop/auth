import {computed, defineComponent, onMounted, reactive, ref} from "vue";
import router from "@/router";
import {getPermissionExpList, getPermissionRequestDetail,} from "@/api/permission";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 申请ID */
const requestId = ref("");

/** 申请信息 */
const requestInfo = reactive({
  requestId: "",
  userId: "",
  username: "",
  reason: "",
  requestTime: "",
  pendingCount: 0,
  approvedCount: 0,
  autoApproveCount: 0,
  rejectedCount: 0,
  totalCount: 0,
  items: [] as any[],
});

/** 过滤后的权限明细列表 */
const statusFilter = ref<string[]>([]);

const filteredItems = computed(() => {
  if (statusFilter.value.length === 0) {
    return requestInfo.items;
  }
  return requestInfo.items.filter((item: any) =>
    statusFilter.value.includes(item.status),
  );
});

/** 状态颜色映射 */
const statusColorMap: Record<string, string> = {
  PENDING: "arcoblue",
  APPROVED: "green",
  AUTO_APPROVED: "cyan",
  REJECTED: "red",
};

/** 状态文本映射 */
const statusTextMap: Record<string, string> = {
  PENDING: "待审批",
  APPROVED: "已批准",
  AUTO_APPROVED: "自动批准",
  REJECTED: "已拒绝",
};

const getStatusColor = (status: string) => statusColorMap[status] || "gray";
const getStatusText = (status: string) => statusTextMap[status] || status;

/** 选中的明细项 */
const selectedItems = ref<string[]>([]);

/** 表格引用 */
const tableRef = ref();

/** 待审批的权限明细 */
const pendingItems = computed(() =>
  requestInfo.items.filter((item: any) => item.status === "PENDING"),
);

/** 是否有待审批的权限 */
const hasPendingItems = computed(() => pendingItems.value.length > 0);

/** 是否全部待审批已选中 */
const isAllPendingSelected = computed(
  () =>
    hasPendingItems.value &&
    pendingItems.value.every((item: any) =>
      selectedItems.value.includes(item.id),
    ),
);

/** 是否部分待审批已选中 */
const isSomePendingSelected = computed(
  () =>
    hasPendingItems.value &&
    pendingItems.value.some((item: any) =>
      selectedItems.value.includes(item.id),
    ) &&
    !isAllPendingSelected.value,
);

/** 切换全选/取消全选待审批 */
const handleToggleSelectAllPending = (val: any) => {
  if (val) {
    // 全选所有待审批
    const pendingIds = pendingItems.value.map((item: any) => item.id);
    const currentSelected = [...selectedItems.value];
    const newSelected = [...new Set([...currentSelected, ...pendingIds])];
    selectedItems.value = newSelected;
  } else {
    // 取消全选
    selectedItems.value = selectedItems.value.filter(
      (id) => !pendingItems.value.some((item: any) => item.id === id),
    );
  }
};

/** 批准/拒绝模态框 */
const approveModalVisible = ref(false);
const approveModalMode = ref<"approve" | "reject">("approve");
const approveFormRef = ref();
const approveForm = reactive({
  itemIds: [] as string[],
  expressionIds: [] as string[],
  rejectReason: "",
});
const approveFormRules = {};
const approveFormSubmitLoading = ref(false);

/** 限制条件列表 */
const expressionList = reactive([] as any[]);
const expressionListPagination = {
  total: 0,
  current: 1,
};
const expressionSearchKeyword = ref("");

/**
 * 获取申请详情
 */
const handleGetRequestDetail = () => {
  getPermissionRequestDetail(requestId.value)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        requestInfo.requestId = data.requestId;
        requestInfo.userId = data.userId;
        requestInfo.username = data.username;
        requestInfo.reason = data.reason;
        requestInfo.requestTime = data.requestTime;
        requestInfo.pendingCount = data.pendingCount;
        requestInfo.approvedCount = data.approvedCount;
        requestInfo.autoApproveCount = data.autoApproveCount;
        requestInfo.rejectedCount = data.rejectedCount;
        requestInfo.totalCount = data.totalCount;
        requestInfo.items = data.items || [];
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限申请详情");
    });
};

/**
 * 打开批准模态框
 */
const handleOpenApproveModal = () => {
  if (selectedItems.value.length === 0) {
    // 未选择权限，询问是否批准全部待审批的
    Modal.confirm({
      title: "确认批准",
      content: `确定要批准全部 ${requestInfo.pendingCount} 项待审批的权限吗？`,
      okText: "确认",
      cancelText: "取消",
      onOk: () => {
        approveModalMode.value = "approve";
        approveForm.itemIds = [...selectedItems.value];
        approveForm.expressionIds = [];
        approveForm.rejectReason = "";
        expressionList.length = 0;
        expressionSearchKeyword.value = "";
        expressionListPagination.current = 1;
        handleGetExpressionList();
        approveModalVisible.value = true;
      },
    });
    return;
  }

  approveModalMode.value = "approve";
  approveForm.itemIds = [...selectedItems.value];
  approveForm.expressionIds = [];
  approveForm.rejectReason = "";
  expressionList.length = 0;
  expressionSearchKeyword.value = "";
  expressionListPagination.current = 1;
  handleGetExpressionList();
  approveModalVisible.value = true;
};

/**
 * 打开拒绝模态框
 */
const handleOpenRejectModal = () => {
  if (selectedItems.value.length === 0) {
    Modal.confirm({
      title: "确认拒绝",
      content: `确定要拒绝全部 ${requestInfo.pendingCount} 项待审批的权限吗？`,
      okButtonProps: {
        status: "danger",
      },
      okText: "确认",
      cancelText: "取消",
      onOk: () => {
        approveModalMode.value = "reject";
        approveForm.itemIds = [...selectedItems.value];
        approveForm.expressionIds = [];
        approveForm.rejectReason = "";
        approveModalVisible.value = true;
      },
    });
    return;
  }

  approveModalMode.value = "reject";
  approveForm.itemIds = [...selectedItems.value];
  approveForm.expressionIds = [];
  approveForm.rejectReason = "";
  approveModalVisible.value = true;
};

/**
 * 关闭批准/拒绝模态框
 */
const handleCloseApproveModal = () => {
  approveFormRef.value?.resetFields();
  approveModalVisible.value = false;
};

/**
 * 获取限制条件列表
 */
const handleGetExpressionList = (page: number = 1) => {
  getPermissionExpList({
    page,
    size: 15,
    keyword: expressionSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        if (page === 1) {
          expressionList.length = 0;
          expressionList.push(...data.list);
        } else {
          expressionList.push(...data.list);
        }
        expressionListPagination.current = data.current;
        expressionListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取限制条件列表");
    });
};

/**
 * 加载更多限制条件
 */
let loadMoreExpressionLoading = false;
const handleLoadMoreExpression = () => {
  if (loadMoreExpressionLoading) return;
  if (expressionList.length < expressionListPagination.total) {
    loadMoreExpressionLoading = true;
    expressionListPagination.current++;
    handleGetExpressionList(expressionListPagination.current);
    loadMoreExpressionLoading = false;
  }
};

/**
 * 搜索限制条件
 */
const handleSearchExpression = () => {
  expressionList.length = 0;
  expressionListPagination.current = 1;
  handleGetExpressionList(1);
};

/**
 * 执行批准或拒绝操作
 */
const handleDoApproveOrReject = (
  isApprove: boolean,
  itemIds: string[],
  expressionIds?: string[],
  rejectReason?: string,
) => {
  approveFormSubmitLoading.value = true;

  const requestData = {
    approve: isApprove,
    requestId: requestId.value,
    itemIds: itemIds.length > 0 ? itemIds : undefined,
    expressionIds: isApprove ? expressionIds : undefined,
    rejectReason: isApprove ? undefined : rejectReason,
  };

  // 动态导入 approvePermissionRequest
  import("@/api/permission").then(({ approvePermissionRequest }) => {
    approvePermissionRequest(requestData)
      .then((result: any) => {
        handleApiSuccess(result, () => {
          Notification.success(isApprove ? "批准成功" : "拒绝成功");
          handleCloseApproveModal();
          handleGetRequestDetail();
          selectedItems.value = [];
        });
      })
      .catch((err: any) => {
        handleApiError(err, isApprove ? "批准权限申请" : "拒绝权限申请");
      })
      .finally(() => {
        approveFormSubmitLoading.value = false;
      });
  });
};

/**
 * 提交批准/拒绝表单
 */
const handleApproveFormSubmit = async () => {
  const validateResult = await approveFormRef.value?.validate();
  if (validateResult) return;

  const isApprove = approveModalMode.value === "approve";
  handleDoApproveOrReject(
    isApprove,
    approveForm.itemIds,
    approveForm.expressionIds,
    approveForm.rejectReason,
  );
};

/**
 * 行选择变化
 */
const handleSelectionChange = (rowKeys: string[]) => {
  selectedItems.value = rowKeys;
};

/**
 * 跳转到用户详情
 */
const handleToUserDetail = (userId: string) => {
  router.push({
    path: "/user/detail",
    query: {
      id: userId,
      active_tab: "user_info",
    },
  });
};

/**
 * 清除选中项
 */
const handleClearSelection = () => {
  selectedItems.value = [];
};

/**
 * 跳转到资源组详情
 */
const handleToResourceGroupDetail = (resourceGroupId: string) => {
  router.push({
    path: "/resource/group/detail",
    query: {
      id: resourceGroupId,
      active_tab: "resource_group_info",
    },
  });
};

/**
 * 跳转到资源详情
 */
const handleToResourceDetail = (resourceId: string) => {
  router.push({
    path: "/permission/resource/detail",
    query: {
      id: resourceId,
      active_tab: "resource_info",
    },
  });
};

/**
 * 跳转到权限详情
 */
const handleToPermissionDetail = (permissionId: string) => {
  router.push({
    path: "/permission/detail",
    query: {
      id: permissionId,
      active_tab: "permission_info",
    },
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      requestId.value = getQueryString("id") || "";
      if (requestId.value) {
        handleGetRequestDetail();
      }
    });

    return {
      handleBack,
      requestId,
      requestInfo,
      filteredItems,
      statusFilter,
      getStatusColor,
      getStatusText,
      selectedItems,
      tableRef,
      handleSelectionChange,
      handleClearSelection,
      handleToggleSelectAllPending,
      isAllPendingSelected,
      isSomePendingSelected,
      hasPendingItems,
      handleOpenApproveModal,
      handleOpenRejectModal,
      approveModalVisible,
      approveModalMode,
      approveFormRef,
      approveForm,
      approveFormRules,
      approveFormSubmitLoading,
      handleCloseApproveModal,
      handleApproveFormSubmit,
      expressionList,
      expressionSearchKeyword,
      handleSearchExpression,
      handleLoadMoreExpression,
      handleToUserDetail,
      handleToResourceGroupDetail,
      handleToResourceDetail,
      handleToPermissionDetail,
    };
  },
});
