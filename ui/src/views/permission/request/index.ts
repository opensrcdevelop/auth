import {getPermissionRequestList} from "@/api/permission";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {defineComponent, onMounted, reactive, ref} from "vue";
import router from "@/router";
import {usePagination} from "@/hooks/usePagination";

/** 权限申请列表 */
const requestList = reactive<any[]>([]);
/** 申请人搜索关键字 */
const searchKeyword = ref<string | undefined>(undefined);
/** 只查看待审批 */
const pendingOnly = ref(false);

let requestListPagination: ReturnType<typeof usePagination>;

/**
 * 获取权限申请列表
 *
 * @param page 页数
 * @param size 条数
 */
const handleGetRequestList = (page: number = 1, size: number = 15) => {
  getPermissionRequestList({
    page,
    size,
    keyword: searchKeyword.value,
    pendingOnly: pendingOnly.value || undefined,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        requestList.length = 0;
        requestList.push(...data.list);
        requestListPagination.updatePagination(
          data.current,
          data.total,
          data.size,
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限申请列表");
    });
};

/**
 * 待审批过滤变化
 *
 * @param checked 是否只查看待审批
 */
const handlePendingOnlyChange = (
  checked: boolean | (string | boolean | number)[],
) => {
  pendingOnly.value = Boolean(checked);
  handleGetRequestList(1, requestListPagination.pagination.pageSize);
};

/**
 * 跳转到用户详情页
 *
 * @param record 权限申请记录
 */
const handleToUserDetail = (record: any) => {
  router.push({
    path: "/user/detail",
    query: {
      id: record.userId,
      active_tab: "user_info",
    },
  });
};

/**
 * 跳转到权限申请详情页
 *
 * @param record 权限申请记录
 */
const handleToRequestDetail = (record: any) => {
  router.push({
    path: "/permission/request/detail",
    query: {
      id: record.requestId,
    },
  });
};

export default defineComponent({
  setup() {
    requestListPagination = usePagination(
      "permissionRequestList",
      ({ page, size }: { page: number; size: number }) => {
        handleGetRequestList(page, size);
      },
    );

    onMounted(() => {
      handleGetRequestList(
        requestListPagination.pagination.current,
        requestListPagination.pagination.pageSize,
      );
    });

    return {
      requestList,
      searchKeyword,
      pendingOnly,
      requestListPagination,
      handleGetRequestList,
      handlePendingOnlyChange,
      handleToUserDetail,
      handleToRequestDetail,
    };
  },
});
