import { defineComponent, ref, reactive, onMounted } from 'vue';
import { useRouter } from 'vue-router';
import { getPendingList, getAllList } from '@/api/adminPermissionRequest';
import { handleApiError, handleApiSuccess } from '@/util/tool';

export default defineComponent({
  name: 'PermissionRequestApproval',
  setup() {
    const router = useRouter();
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

    // 查看详情 - 跳转到详情页
    const handleViewDetail = (record: any) => {
      router.push(`/permission/request/detail?id=${record.requestId}`);
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
    };
  },
});
