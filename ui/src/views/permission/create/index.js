import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { createPermission } from "@/api/permission";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/** 创建权限表单 */
var createPermissionFormRef = ref();
var createPermissionForm = reactive({
    name: undefined,
    code: undefined,
    desc: undefined,
    resourceId: undefined,
});
var createPermissionFormRules = {
    name: [{ required: true, message: "权限名称未填写" }],
    code: [
        { required: true, message: "权限标识未填写" },
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
 * 重置创建权限表单
 */
var handleResetCreatePermissionForm = function () {
    createPermissionFormRef.value.resetFields();
};
/**
 * 提交创建权限表单
 */
var handleCreatePermissionFormSubmit = function (formData) {
    createPermission(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreatePermissionForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建权限");
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            var globaleVariables = useGlobalVariablesStore();
            createPermissionForm.resourceId = globaleVariables.resourceId;
        });
        return {
            handleBack: handleBack,
            createPermissionFormRef: createPermissionFormRef,
            createPermissionForm: createPermissionForm,
            createPermissionFormRules: createPermissionFormRules,
            handleResetCreatePermissionForm: handleResetCreatePermissionForm,
            handleCreatePermissionFormSubmit: handleCreatePermissionFormSubmit,
        };
    },
});
