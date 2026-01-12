import { createDict } from "@/api/dict";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, reactive, ref } from "vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/** 创建字典表单 */
var createDictFormRef = ref();
var createDictForm = reactive({
    name: undefined,
    code: undefined,
    desc: undefined,
});
var createDictFormRules = {
    name: [{ required: true, message: "字典名称未填写" }],
    code: [
        { required: true, message: "字典标识未填写" },
        {
            validator: function (value, cb) {
                if (value && !/^[A-Za-z0-9-\_]+$/.test(value)) {
                    cb("只允许包含英文字母、数字、下划线_、横线-");
                }
                else {
                    cb();
                }
            },
        },
    ],
};
/**
 * 提交创建字典表单
 */
var handleCreateDictFormSubmit = function (formData) {
    createDict(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreateDictForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建字典");
    });
};
/**
 * 重置创建字典表单
 */
var handleResetCreateDictForm = function () {
    createDictFormRef.value.resetFields();
};
export default defineComponent({
    setup: function () {
        return {
            handleBack: handleBack,
            createDictFormRef: createDictFormRef,
            createDictForm: createDictForm,
            createDictFormRules: createDictFormRules,
            handleCreateDictFormSubmit: handleCreateDictFormSubmit,
            handleResetCreateDictForm: handleResetCreateDictForm,
        };
    },
});
