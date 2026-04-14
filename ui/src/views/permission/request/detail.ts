import { ref, reactive, computed } from 'vue';
import { useRoute, useRouter } from 'vue-router';
import { Message } from '@arco-design/web-vue';
import { getRequestDetail, approveRequest, rejectRequest } from '@/api/adminPermissionRequest';
import { getPermissionExpList } from '@/api/permission';

const route = useRoute();
const router = useRouter();
const requestId = computed(() => route.query.id as string);

// 页面状态
const loading = ref(false);
const detail = ref<any>(null);

// 选中项
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

// computed
const hasPendingItems = computed(() => {
  return detail.value?.items?.some((item: any) => item.status === 'PENDING') ?? false;
});

const pendingItemCount = computed(() => {
  return detail.value?.items?.filter((item: any) => item.status === 'PENDING').length ?? 0;
});

const allSelected = computed(() => {
  const pendingItems = detail.value?.items?.filter((item: any) => item.status === 'PENDING') ?? [];
  return pendingItems.length > 0 && selectedItemIds.value.length === pendingItems.length;
});

const indeterminate = computed(() => {
  const pendingItems = detail.value?.items?.filter((item: any) => item.status === 'PENDING') ?? [];
  return selectedItemIds.value.length > 0 && selectedItemIds.value.length < pendingItems.length;
});

// 状态映射
const STATUS_COLOR_MAP: Record<string, string> = {
  PENDING: 'arcoblue',
  APPROVED: 'green',
  REJECTED: 'red',
};

const STATUS_TEXT_MAP: Record<string, string> = {
  PENDING: '待审批',
  APPROVED: '已批准',
  REJECTED: '已拒绝',
};

// 方法
const loadDetail = async () => {
  if (!requestId.value) return;
  loading.value = true;
  try {
    const res: any = await getRequestDetail(requestId.value);
    if (res.data) {
      detail.value = res.data;
    }
  } catch (err: any) {
    Message.error(err.message || '加载详情失败');
  } finally {
    loading.value = false;
  }
};

const loadExpressionList = async () => {
  try {
    const res: any = await getPermissionExpList({});
    if (res.data) {
      expressionList.value = res.data;
    }
  } catch (err) {
    console.error('加载限制条件失败', err);
  }
};

const handleItemCheck = (itemId: string) => {
  const index = selectedItemIds.value.indexOf(itemId);
  if (index > -1) {
    selectedItemIds.value.splice(index, 1);
  } else {
    selectedItemIds.value.push(itemId);
  }
};

const handleSelectAllChange = (checked: boolean) => {
  if (checked) {
    selectedItemIds.value = detail.value?.items
      ?.filter((item: any) => item.status === 'PENDING')
      ?.map((item: any) => item.itemId) ?? [];
  } else {
    selectedItemIds.value = [];
  }
};

const handleBatchApprove = () => {
  if (selectedItemIds.value.length === 0) {
    Message.warning('请选择要批准的权限');
    return;
  }
  approveForm.itemIds = [...selectedItemIds.value];
  approveForm.expressionIds = [];
  approveForm.priority = undefined;
  loadExpressionList();
  approveModalVisible.value = true;
};

const handleSingleApprove = (itemId: string) => {
  approveForm.itemIds = [itemId];
  approveForm.expressionIds = [];
  approveForm.priority = undefined;
  loadExpressionList();
  approveModalVisible.value = true;
};

const handleApproveSubmit = async (done: Function) => {
  approveLoading.value = true;
  try {
    await approveRequest(requestId.value, {
      itemIds: approveForm.itemIds,
      expressionIds: approveForm.expressionIds.length > 0 ? approveForm.expressionIds : undefined,
      priority: approveForm.priority,
    });
    Message.success('批准成功');
    approveModalVisible.value = false;
    await loadDetail();
    selectedItemIds.value = [];
    done(true);
  } catch (err: any) {
    Message.error(err.message || '批准失败');
    done(false);
  } finally {
    approveLoading.value = false;
  }
};

const handleApproveCancel = () => {
  approveModalVisible.value = false;
};

const handleBatchReject = () => {
  if (selectedItemIds.value.length === 0) {
    Message.warning('请选择要拒绝的权限');
    return;
  }
  rejectForm.itemIds = [...selectedItemIds.value];
  rejectForm.rejectReason = '';
  rejectModalVisible.value = true;
};

const handleSingleReject = (itemId: string) => {
  rejectForm.itemIds = [itemId];
  rejectForm.rejectReason = '';
  rejectModalVisible.value = true;
};

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
  rejectLoading.value = true;
  try {
    await rejectRequest(requestId.value, {
      itemIds: rejectForm.itemIds,
      rejectReason: rejectForm.rejectReason,
    });
    Message.success('拒绝成功');
    rejectModalVisible.value = false;
    await loadDetail();
    selectedItemIds.value = [];
    done(true);
  } catch (err: any) {
    Message.error(err.message || '拒绝失败');
    done(false);
  } finally {
    rejectLoading.value = false;
  }
};

const handleRejectCancel = () => {
  rejectModalVisible.value = false;
};

export {
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
  pendingItemCount,
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
};
