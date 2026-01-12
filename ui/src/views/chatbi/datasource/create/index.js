import router from "@/router";
import { defineComponent, reactive, ref } from "vue";
import { DS_TYPE_LIST } from "../constants";
import { createDataSourceConf, testDataSourceConn } from "@/api/chatbi";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var dataSourceTypeList = DS_TYPE_LIST;
/** 创建数据源表单 */
var createDataSourceFormRef = ref();
var createDataSourceForm = reactive({
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
var createDataSourceFormRules = {
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
var handleCreateDataSourceFormSubmit = function (formData) {
    createDataSourceConf(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreateDataSourceForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建数据源");
    });
};
/**
 * 重置创建数据源表单
 */
var handleResetCreateDataSourceForm = function () {
    createDataSourceFormRef.value.resetFields();
};
/**
 * 测试数据源连接
 */
var hanleTestConn = function () {
    createDataSourceFormRef.value.validate(function (errors) {
        if (!errors) {
            testDataSourceConn({
                type: createDataSourceForm.type,
                database: createDataSourceForm.database,
                host: createDataSourceForm.host,
                port: createDataSourceForm.port,
                username: createDataSourceForm.username,
                password: createDataSourceForm.password,
            })
                .then(function (result) {
                handleApiSuccess(result, function (data) {
                    if (data.connected) {
                        Notification.success("连接成功");
                    }
                    else {
                        Notification.error("连接失败");
                    }
                });
            })
                .catch(function (err) {
                handleApiError(err, "测试数据源连接");
            });
        }
    });
};
export default defineComponent({
    setup: function () {
        return {
            handleBack: handleBack,
            dataSourceTypeList: dataSourceTypeList,
            createDataSourceFormRef: createDataSourceFormRef,
            createDataSourceForm: createDataSourceForm,
            createDataSourceFormRules: createDataSourceFormRules,
            handleCreateDataSourceFormSubmit: handleCreateDataSourceFormSubmit,
            handleResetCreateDataSourceForm: handleResetCreateDataSourceForm,
            hanleTestConn: hanleTestConn,
        };
    },
});
