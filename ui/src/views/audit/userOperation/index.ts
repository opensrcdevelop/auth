import {getObjChanges, getUserOperationLogs,} from "@/api/auditLog";
import {usePagination} from "@/hooks/usePagination";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import dayjs from "dayjs";
import {defineComponent, reactive, ref} from "vue";

/**
 * 操作类型
 */
const operationTypes = [
  { value: 1, label: "修改个人信息" },
  { value: 2, label: "修改密码" },
  { value: 3, label: "绑定邮箱" },
  { value: 4, label: "解绑邮箱" },
  { value: 5, label: "绑定手机号" },
  { value: 6, label: "解绑手机号" },
  { value: 7, label: "绑定第三方账号" },
  { value: 8, label: "解绑第三方账号" },
  { value: 9, label: "绑定 MFA 设备" },
  { value: 10, label: "重置密码" },
  { value: 11, label: "ChatBI 对话"},
  { value: 12, label: "ChatBI 问答反馈"},
  { value: 13, label: "ChatBI 删除对话历史"},
  { value: 14, label: "ChatBI 更新对话历史"},
];

/**
 * 表格列
 */
const columns = reactive([
  {
    label: "用户",
    key: "username",
    value: (data) => data.username,
    visible: true,
    editable: false,
    width: 100,
    ellipsis: false,
  },
  {
    label: "详情",
    key: "detail",
    visible: true,
    editable: false,
    width: 380,
    ellipsis: true,
  },
  {
    label: "类型",
    key: "type",
    visible: false,
    editable: true,
    width: 100,
    ellipsis: false,
  },
  {
    label: "结果",
    key: "result",
    visible: true,
    editable: true,
    ellipsis: false,
    width: 60,
  },
  {
    label: "IP",
    key: "ip",
    visible: false,
    editable: true,
    ellipsis: false,
    width: 100,
  },
  {
    label: "IP 归属地",
    key: "ipRegion",
    visible: false,
    editable: true,
    ellipsis: false,
    width: 120,
  },
  {
    label: "设备类型",
    key: "deviceType",
    visible: false,
    editable: true,
    ellipsis: false,
    width: 100,
  },
  {
    label: "OS 类型",
    key: "osType",
    visible: false,
    editable: true,
    ellipsis: false,
    width: 140,
  },
  {
    label: "浏览器类型",
    key: "browserType",
    visible: false,
    editable: true,
    ellipsis: false,
    width: 140,
  },
  {
    label: "时间",
    key: "time",
    visible: true,
    editable: true,
    ellipsis: false,
    width: 180,
  },
]);

/**
 * 获取 Dayjs 对象
 */
const getDayjs = (current: Date) => {
  return dayjs(current);
};

// 用户操作日志
const userOperationLogs = reactive([]);
let userOperationLogsPagination;
const searchKeyword = ref(null);
const operationType = ref(null);
const dateRange = ref(null);

/**
 * 获取用户操作日志
 */
const handleGetUserOperationLogs = (page: number = 1, size: number = 15) => {
  const params = {
    page,
    size,
    keyword: undefined,
    type: undefined,
    start: undefined,
    end: undefined,
  };

  if (searchKeyword.value) {
    params.keyword = searchKeyword.value.trim();
  }

  if (operationType.value) {
    params.type = operationType.value;
  }

  if (dateRange.value) {
    params.start = dateRange.value[0];
    params.end = dateRange.value[1];
  }

  getUserOperationLogs(params)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userOperationLogs.length = 0;
        userOperationLogs.push(...data.list);

        userOperationLogsPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户操作日志");
    });
};

/**
 * 跳转用户详情
 */
const handleToUserDetail = (id: string) => {
  router.push({
    path: "/user/detail",
    query: {
      id,
    },
  });
};

/**
 * 对象变更日志
 */
const objChanges = reactive([]);
const objChangesModalVisible = ref(false);
const handleGetObjChanges = (auditLogId: string) => {
  getObjChanges(auditLogId)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        objChanges.length = 0;
        objChanges.push(...data.changes);
        objChangesModalVisible.value = true;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取对象变更日志");
    });
};

export default defineComponent({
  setup() {
    userOperationLogsPagination = usePagination(
      "userOperationLogs",
      ({ page, size }) => {
        handleGetUserOperationLogs(page, size);
      }
    );

    return {
      operationTypes,
      columns,
      searchKeyword,
      operationType,
      dateRange,
      userOperationLogs,
      userOperationLogsPagination,
      getDayjs,
      handleGetUserOperationLogs,
      handleToUserDetail,
      handleGetObjChanges,
      objChanges,
      objChangesModalVisible,
    };
  },
});
