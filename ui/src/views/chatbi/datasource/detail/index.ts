import {getDataSourceConfDetail, testDataSourceConn, updateDataSourceConf,} from "@/api/chatbi";
import router from "@/router";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {defineComponent, onMounted, reactive, ref} from "vue";
import {DS_TYPE_LIST} from "../constants";
import {Notification} from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("data_source_info");

/**
 * tab 切换事件
 */
const handleTabChange = (tabKey: string) => {
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

export default defineComponent({
  setup() {
    const resourceGroupId = getQueryString("id");

    onMounted(() => {
      activeTab.value = getQueryString("active_tab") || "data_source_info";
      handleTabInit(activeTab.value, resourceGroupId);
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
    };
  },
});
