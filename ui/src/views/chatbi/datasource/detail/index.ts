import {
  batchUpdateTable,
  getDataSourceConfDetail,
  getTableList,
  testDataSourceConn,
  updateDataSourceConf,
} from "@/api/chatbi";
import router from "@/router";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {computed, defineComponent, onMounted, reactive, ref} from "vue";
import {DS_TYPE_LIST} from "../constants";
import {Notification} from "@arco-design/web-vue";
import {usePagination} from "@/hooks/usePagination";
import TextEditorModal from "../../modal/TextEditorModal.vue";
import MdEditorModal from "../../modal/MdEditorModal.vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  if (detectRowChanges()) {
    router.back();
  }
};

const activeTab = ref("data_source_info");

/**
 * tab 切换事件
 */
const handleTabChange = (tabKey: string) => {
  if (!detectRowChanges()) return;
  router.replace({
    query: {
      ...router.currentRoute.value.query,
      active_tab: tabKey,
    },
  });
  activeTab.value = tabKey;
  handleTabInit(tabKey);
};

const handleTabInit = (tabKey: string, id: string = dataSourceId.value) => {
  switch (tabKey) {
    case "data_source_info":
      handleGetDataSourceDetail(id);
      break;
    case "table_list":
      handleGetDataSourceDetail(id);
      handleGetTableList(id);
      break;
  }
};

const dataSourceTypeList = DS_TYPE_LIST;

const dataSourceId = ref("");
const dataSourceName = ref("");

/** 数据源信息表单 */
const dataSourceInfoFormRef = ref();
const dataSourceInfoForm = reactive({
  id: undefined,
  name: undefined,
  type: undefined,
  database: undefined,
  schema: undefined,
  host: undefined,
  port: undefined,
  username: undefined,
  password: undefined,
  jdbcParams: undefined,
  desc: undefined,
});
const dataSourceInfoFormRules = {
  name: [{ required: true, message: "数据源名称未填写" }],
  host: [{ required: true, message: "主机地址未填写" }],
  port: [{ required: true, message: "端口号未填写" }],
  username: [{ required: true, message: "用户名未填写" }],
  password: [{ required: true, message: "密码未填写" }],
};

/**
 * 获取数据源详情
 */
const handleGetDataSourceDetail = (id: string) => {
  getDataSourceConfDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dataSourceId.value = data.id;
        dataSourceName.value = data.name;

        dataSourceInfoForm.id = data.id;
        dataSourceInfoForm.name = data.name;
        dataSourceInfoForm.type = data.type;
        dataSourceInfoForm.database = data.database;
        dataSourceInfoForm.schema = data.schema;
        dataSourceInfoForm.host = data.host;
        dataSourceInfoForm.port = data.port;
        dataSourceInfoForm.username = data.username;
        dataSourceInfoForm.password = data.password;
        dataSourceInfoForm.jdbcParams = data.jdbcParams;
        dataSourceInfoForm.desc = data.desc;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取数据源详情");
    });
};

/**
 * 测试数据源连接
 */
const hanleTestConn = () => {
  dataSourceInfoFormRef.value.validate((errors) => {
    if (!errors) {
      testDataSourceConn({
        type: dataSourceInfoForm.type,
        database: dataSourceInfoForm.database,
        host: dataSourceInfoForm.host,
        port: dataSourceInfoForm.port,
        username: dataSourceInfoForm.username,
        password: dataSourceInfoForm.password,
      })
        .then((result: any) => {
          handleApiSuccess(result, (data: any) => {
            if (data.connected) {
              Notification.success("连接成功");
            } else {
              Notification.error("连接失败");
            }
          });
        })
        .catch((err: any) => {
          handleApiError(err, "测试数据源连接");
        });
    }
  });
};

/**
 * 提交数据源信息表单
 */
const handleDataSourceInfoFormSubmit = (formData: any) => {
  updateDataSourceConf(formData)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        Notification.success("保存成功");
        handleGetDataSourceDetail(dataSourceId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新数据源配置");
    });
};

/**
 * 重置数据源信息表单
 */
const handleResetDataSourceInfoForm = () => {
  dataSourceInfoFormRef.value.resetFields();
  handleGetDataSourceDetail(dataSourceId.value);
};

/** 数据表列表 */
const tableList = reactive([]);
const tableSearchKeyword = ref("");
let tableListPagination;

const handleTableListPageChange = (page: number) => {
  tableListPagination.handlePageChange(page, detectRowChanges);
};

const handleTableListPageSizeChange = (size: number) => {
  tableListPagination.handlePageSizeChange(size, detectRowChanges);
};

const handleGetTableList = (
  id: string = dataSourceId.value,
  page: number = 1,
  size: number = 15
) => {
  getTableList(id, {
    page,
    size,
    keyword: tableSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        tableList.length = 0;

        data.list.forEach((item) => {
          tableList.push({
            ...item,
            _isHovering: false,
            _originalData: JSON.parse(JSON.stringify(item)),
          });
        });

        tableListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取数据表列表");
    });
};

/**
 * 判断行数据是否被修改过
 */
const isRowModified = (rowData: any) => {
  if (!rowData._originalData) return false;

  const { _isHovering, _originalData, ...currentData } = rowData;
  return JSON.stringify(currentData) !== JSON.stringify(_originalData);
};

const detectRowChanges = () => {
  if (!saveBtnDisabled.value) {
    return confirm("有未保存的编辑，是否离开当前页面？");
  }
  return true;
};

/**
 * 鼠标悬停
 */
const handleHoverIn = (rowData: any) => {
  if (isRowModified(rowData)) {
    rowData._isHovering = true;
  }
};

/**
 * 鼠标移出
 */
const handleHoverOut = (rowData: any) => {
  if (isRowModified(rowData)) {
    rowData._isHovering = false;
  }
};

/**
 * 重置行数据
 */
const handleResetRow = (record: any) => {
  if (record._originalData) {
    const index = tableList.findIndex((item) => item.id === record.id);
    if (index !== -1) {
      const originalData = JSON.parse(JSON.stringify(record._originalData));
      originalData._originalData = record._originalData;
      originalData._isHovering = false;
      tableList[index] = originalData;
    }
  }
};

/** 保存按钮禁用状态 */
const saveBtnDisabled = computed(() => {
  if (!tableList) return true;
  if (tableList.find((item) => isRowModified(item))) {
    return false;
  }
  return true;
});

/** 表注释编辑对话框 */
const textEditorModalVisible = ref(false);
const textEditorModalContent = ref("");
const textEditorModalTitle = ref("");
const textEditorModalRecordId = ref("");

/**
 * 打开表注释编辑对话框
 */
const handleOpenTextEditorModal = (record: any) => {
  textEditorModalContent.value = record.remark;
  textEditorModalVisible.value = true;
  textEditorModalTitle.value = "编辑表注释 - " + record.name;
  textEditorModalRecordId.value = record.id;
};

/**
 * 关闭表注释编辑对话框
 */
const handleCloseTextEditorModal = () => {
  textEditorModalVisible.value = false;
  textEditorModalContent.value = "";
  textEditorModalTitle.value = "";
  textEditorModalRecordId.value = "";
};

/**
 * 表注释编辑对话框确认
 */
const handleTextEditorModalConfirm = (newContent: string) => {
  const targetRecord = tableList.find(
    (item) => item.id === textEditorModalRecordId.value
  );
  if (targetRecord) {
    targetRecord.remark = newContent;
  }
  handleCloseTextEditorModal();
};

/** 表补充信息编辑对话框 */
const mdEditorModalVisible = ref(false);
const mdEditorModalContent = ref("");
const mdEditorModalTitle = ref("");
const mdEditorModalRecordId = ref("");

/**
 * 打开表补充信息编辑对话框
 */
const handleOpenMdEditorModal = (record: any) => {
  mdEditorModalContent.value = record.additionalInfo;
  mdEditorModalVisible.value = true;
  mdEditorModalTitle.value = "编辑表补充信息 - " + record.name;
  mdEditorModalRecordId.value = record.id;
};

/**
 * 关闭表补充信息编辑对话框
 */
const handleCloseMdEditorModal = () => {
  mdEditorModalVisible.value = false;
  mdEditorModalContent.value = "";
  mdEditorModalTitle.value = "";
  mdEditorModalRecordId.value = "";
};

/**
 * 表补充信息编辑对话框确认
 */
const handleMdEditorModalConfirm = (newContent: string) => {
  const targetRecord = tableList.find(
    (item) => item.id === mdEditorModalRecordId.value
  );
  if (targetRecord) {
    targetRecord.additionalInfo = newContent;
  }
  handleCloseMdEditorModal();
};

/**
 * 保存表数据
 */
const handleSaveTableList = () => {
  const list = tableList
    .filter((item) => isRowModified(item))
    .map((item) => {
      const { _isHovering, _originalData, ...currentData } = item;
      return currentData;
    });
  batchUpdateTable({
    list,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetTableList(dataSourceId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "批量更新表");
    });
};

export default defineComponent({
  components: {
    TextEditorModal,
    MdEditorModal,
  },
  setup() {
    const dataSourceId = getQueryString("id");

    tableListPagination = usePagination("tableList", ({ page, size }) => {
      handleGetTableList(dataSourceId, page, size);
    });

    onMounted(() => {
      activeTab.value = getQueryString("active_tab") || "data_source_info";
      handleTabInit(activeTab.value, dataSourceId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      dataSourceTypeList,
      dataSourceId,
      dataSourceName,
      dataSourceInfoFormRef,
      dataSourceInfoForm,
      dataSourceInfoFormRules,
      hanleTestConn,
      handleDataSourceInfoFormSubmit,
      handleResetDataSourceInfoForm,
      tableList,
      tableListPagination,
      tableSearchKeyword,
      handleGetTableList,
      handleTableListPageChange,
      handleTableListPageSizeChange,
      isRowModified,
      handleHoverIn,
      handleHoverOut,
      handleResetRow,
      saveBtnDisabled,
      textEditorModalVisible,
      textEditorModalTitle,
      textEditorModalContent,
      handleOpenTextEditorModal,
      handleCloseTextEditorModal,
      handleTextEditorModalConfirm,
      mdEditorModalVisible,
      mdEditorModalTitle,
      mdEditorModalContent,
      handleOpenMdEditorModal,
      handleCloseMdEditorModal,
      handleMdEditorModalConfirm,
      handleSaveTableList,
    };
  },
});
