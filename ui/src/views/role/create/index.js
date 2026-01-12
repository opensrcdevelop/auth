import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { createRole } from "@/api/role";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/** 创建角色表单 */
var createRoleFormRef = ref();
var createRoleForm = reactive({
    name: "",
    code: "",
    desc: "",
});
var createRoleFormRules = {
    name: [{ required: true, message: "角色名称未填写" }],
    code: [
        { required: true, message: "角色标识未填写" },
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
 * 重置创建角色表单
 */
var handleResetCreateRoleForm = function () {
    createRoleFormRef.value.resetFields();
};
/**
 * 提交创建角色表单
 */
var handleCreateRoleFormSubmit = function (formData) {
    createRole(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreateRoleForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建角色");
    });
};
export default defineComponent({
    setup: function () {
        return {
            handleBack: handleBack,
            createRoleFormRef: createRoleFormRef,
            createRoleForm: createRoleForm,
            createRoleFormRules: createRoleFormRules,
            handleResetCreateRoleForm: handleResetCreateRoleForm,
            handleCreateRoleFormSubmit: handleCreateRoleFormSubmit,
        };
    },
});
