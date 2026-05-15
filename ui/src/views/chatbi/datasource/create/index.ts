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
  database: undefined,
  schema: undefined,
  host: undefined,
  port: undefined,
  username: undefined,
  password: undefined,
  jdbcParams: undefined,
  desc: undefined,
  // DUCKDB 专用字段
  s3Bucket: undefined,
  s3Endpoint: undefined,
  s3Region: undefined,
  s3AccessKey: undefined,
  s3SecretKey: undefined,
});
const createDataSourceFormRules = computed(() => ({
  name: [{ required: true, message: "数据源名称未填写" }],
  type: [{ required: true, message: "数据源类型未选择" }],
  database: [{ required: true, message: "数据库未填写" }],
  // 数据库类型：host/port/username/password 必填
  // DUCKDB 类型：这些字段隐藏，不需要验证
  host: [{ required: !isDuckDB.value, message: "主机地址未填写" }],
  port: [{ required: !isDuckDB.value, message: "端口号未填写" }],
  username: [{ required: !isDuckDB.value, message: "用户名未填写" }],
  password: [{ required: !isDuckDB.value, message: "密码未填写" }],
  // DUCKDB 类型：S3 字段必填
  s3Bucket: [{ required: isDuckDB.value, message: "S3 Bucket 未填写" }],
  s3Region: [{ required: isDuckDB.value, message: "S3 Region 未填写" }],
  s3AccessKey: [{ required: isDuckDB.value, message: "S3 Access Key 未填写" }],
  s3SecretKey: [{ required: isDuckDB.value, message: "S3 Secret Key 未填写" }],
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
      };

      if (isDuckDB.value) {
        // DUCKDB 类型：使用 S3 配置测试
        connData.database = createDataSourceForm.s3Bucket;
        connData.s3AccessKey = createDataSourceForm.s3AccessKey;
        connData.s3SecretKey = createDataSourceForm.s3SecretKey;
        connData.s3Endpoint = createDataSourceForm.s3Endpoint;
        connData.s3Region = createDataSourceForm.s3Region;
      } else {
        // 数据库类型：使用数据库配置测试
        connData.database = createDataSourceForm.database;
        connData.host = createDataSourceForm.host;
        connData.port = createDataSourceForm.port;
        connData.username = createDataSourceForm.username;
        connData.password = createDataSourceForm.password;
      }

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
