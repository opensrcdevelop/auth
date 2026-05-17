import router from "@/router";
import {computed, defineComponent, reactive, ref} from "vue";
import {DS_TYPE_LIST} from "../constants";
import {createDataSourceConf, testDataSourceConn} from "@/api/chatbi";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const dataSourceTypeList = DS_TYPE_LIST;

/** 判断是否为 DuckDB 类型 */
const isDuckDB = computed(() => createDataSourceForm.type === "DUCKDB");

/** 创建数据源表单 */
const createDataSourceFormRef = ref();
const createDataSourceForm = reactive({
  name: undefined,
  type: undefined,
  // 非 DuckDB 使用
  database: undefined,
  schema: undefined,
  host: undefined,
  port: undefined,
  username: undefined,
  password: undefined,
  jdbcParams: undefined,
  desc: undefined,
});
const createDataSourceFormRules = computed(() => ({
  name: [{ required: true, message: "数据源名称未填写" }],
  type: [{ required: true, message: "数据源类型未选择" }],
  // 非 DuckDB 必填字段
  database: [{ required: !isDuckDB.value, message: "数据库未填写" }],
  host: [{ required: !isDuckDB.value, message: "主机地址未填写" }],
  port: [{ required: !isDuckDB.value, message: "端口号未填写" }],
  username: [{ required: !isDuckDB.value, message: "用户名未填写" }],
  password: [{ required: !isDuckDB.value, message: "密码未填写" }],
}));

/**
 * 提交创建数据源表单
 */
const handleCreateDataSourceFormSubmit = (formData: any) => {
  createDataSourceConf(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateDataSourceForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建数据源");
    });
};

/**
 * 重置创建数据源表单
 */
const handleResetCreateDataSourceForm = () => {
  createDataSourceFormRef.value.resetFields();
};

/**
 * 测试数据源连接
 */
const hanleTestConn = () => {
  createDataSourceFormRef.value.validate((errors) => {
    if (!errors) {
      const connData: any = {
        type: createDataSourceForm.type,
        database: createDataSourceForm.database,
        host: createDataSourceForm.host,
        port: createDataSourceForm.port,
        username: createDataSourceForm.username,
        password: createDataSourceForm.password,
      };

      testDataSourceConn(connData)
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

export default defineComponent({
  setup() {
    return {
      handleBack,
      dataSourceTypeList,
      createDataSourceFormRef,
      createDataSourceForm,
      createDataSourceFormRules,
      handleCreateDataSourceFormSubmit,
      handleResetCreateDataSourceForm,
      hanleTestConn,
      isDuckDB,
    };
  },
});
