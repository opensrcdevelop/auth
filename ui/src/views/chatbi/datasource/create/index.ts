import router from "@/router";
import {defineComponent, reactive, ref} from "vue";
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
});
const createDataSourceFormRules = {
  name: [{ required: true, message: "数据源名称未填写" }],
  type: [{ required: true, message: "数据源类型未选择" }],
  database: [{ required: true, message: "数据库未填写" }],
  host: [{ required: true, message: "主机地址未填写" }],
  port: [{ required: true, message: "端口号未填写" }],
  username: [{ required: true, message: "用户名未填写" }],
  password: [{ required: true, message: "密码未填写" }],
};

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
      testDataSourceConn({
        type: createDataSourceForm.type,
        database: createDataSourceForm.database,
        host: createDataSourceForm.host,
        port: createDataSourceForm.port,
        username: createDataSourceForm.username,
        password: createDataSourceForm.password,
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
    };
  },
});
