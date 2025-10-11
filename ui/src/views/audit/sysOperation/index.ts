import {getObjChanges, getSysOperationLogs} from "@/api/auditLog";
import {usePagination} from "@/hooks/usePagination";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import dayjs from "dayjs";
import {defineComponent, reactive, ref} from "vue";

/**
 * 操作类型
 */
const operationTypes = [
  { value: 1, label: "创建" },
  { value: 2, label: "修改" },
  { value: 3, label: "删除" },
];

/**
 * 资源类型
 */
const resourceTypes = [
  { id: "14ee7b7e-db4c-40cc-b93e-d38969be5542", name: "授权条件" },
  { id: "75a3dee9-a95f-4ad3-a32a-a7f6d34c0050", name: "客户端" },
  { id: "22c26616-ba71-4258-a9b4-0901cc3285b7", name: "字典" },
  { id: "ab15084d-285e-4a2d-bde2-d968a504e7a5", name: "字典数据" },
  { id: "97e86c03-4cea-4893-b78a-86d3af30cf0b", name: "身份源提供商" },
  { id: "bb4a7fe7-719d-49fa-bd73-597df6bc5ac3", name: "JWT 设置" },
  { id: "3c290ba3-b34a-4a42-8df9-bc8939d999e8", name: "消息设置" },
  { id: "931848a2-beb6-444e-a7d7-72e909553b00", name: "OIDC Claim" },
  { id: "0f35efeb-3f5a-4e22-84c4-4b4a08b6717c", name: "OIDC Scope" },
  { id: "2d2225a8-5143-4465-ab76-06b101abff8f", name: "密码策略" },
  { id: "75c35dc3-1996-48ab-be27-e4078f86a559", name: "权限" },
  { id: "79a30d3a-0fde-4087-a307-619cc0c56b17", name: "限制条件" },
  { id: "01982839-ae11-75ae-8680-4bb5e0bbe83f", name: "限制条件模板" },
  { id: "edd2a541-f482-45cd-9842-c1ebf43c346c", name: "资源" },
  { id: "911e08a0-d91a-4c66-8a7d-c8fda2c79c69", name: "资源组" },
  { id: "4d367bc0-d043-402c-a1d5-d4e5c55c9e23", name: "角色" },
  { id: "97392350-5214-4dbb-83e8-45b678ce145e", name: "用户" },
  { id: "da2c6573-d236-4e4d-96a4-85c517b72c59", name: "用户属性" },
  { id: "1624ca73-e656-48d9-800e-b5762b51d7c5", name: "用户组" },
  { id: "055a8115-8be2-498e-a6f9-ab8a34f5dd0e", name: "租户" },
  { id: "0199c427-9df0-7fbe-bfe4-de1a8cbebbfa", name: "ChatBI 问数" },
  { id: "0199c923-b73d-73db-a654-76ddd12dd408", name: "ChatBI 问数-数据源" },
  { id: "0199c926-f7e6-79d2-a864-f234ce930b07", name: "ChatBI 问数-模型提供商" },
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
    label: "资源类型",
    key: "resourceId",
    visible: false,
    editable: true,
    width: 120,
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

// 系统操作日志
const sysOperationLogs = reactive([]);
let sysOperationLogsPagination;
const searchKeyword = ref(null);
const operationType = ref(null);
const resourceType = ref(null);
const dateRange = ref(null);

/**
 * 获取系统操作日志
 */
const handleGetSysOperationLogs = (page: number = 1, size: number = 15) => {
  const params = {
    page,
    size,
    keyword: undefined,
    type: undefined,
    resourceId: undefined,
    start: undefined,
    end: undefined,
  };

  if (searchKeyword.value) {
    params.keyword = searchKeyword.value.trim();
  }

  if (operationType.value) {
    params.type = operationType.value;
  }

  if (resourceType.value) {
    params.resourceId = resourceType.value;
  }

  if (dateRange.value) {
    params.start = dateRange.value[0];
    params.end = dateRange.value[1];
  }

  getSysOperationLogs(params)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        sysOperationLogs.length = 0;
        sysOperationLogs.push(...data.list);

        sysOperationLogsPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取系统操作日志");
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
 * 跳转资源详情
 */
const handleToResourceDetail = (id: string) => {
  router.push({
    path: "/permission/resource/detail",
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
    sysOperationLogsPagination = usePagination(
      "sysOperationLogs",
      ({ page, size }) => {
        handleGetSysOperationLogs(page, size);
      }
    );

    return {
      operationTypes,
      resourceTypes,
      columns,
      searchKeyword,
      operationType,
      resourceType,
      dateRange,
      sysOperationLogs,
      sysOperationLogsPagination,
      getDayjs,
      handleGetSysOperationLogs,
      handleToUserDetail,
      handleToResourceDetail,
      handleGetObjChanges,
      objChanges,
      objChangesModalVisible,
    };
  },
});
