import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { createResourceGroup } from "@/api/resourceGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/** 创建资源组表单 */
var createResourceGroupFormRef = ref();
var createResourceGroupForm = reactive({
    name: undefined,
    code: undefined,
    desc: undefined,
});
var createResourceGroupFormRules = {
    name: [{ required: true, message: "资源组名称未填写" }],
    code: [
        { required: true, message: "资源组标识未填写" },
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
    resourceGroupId: [{ required: true, message: "资源组未选择" }],
};
/**
 * 提交创建资源组表单
 */
var handleCreateResourceGroupFormSubmit = function (formData) {
    createResourceGroup(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreateResourceGroupForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建资源组");
    });
};
/**
 * 重置创建资源组表单
 */
var handleResetCreateResourceGroupForm = function () {
    createResourceGroupFormRef.value.resetFields();
};
export default defineComponent({
    setup: function () {
        return {
            handleBack: handleBack,
            createResourceGroupFormRef: createResourceGroupFormRef,
            createResourceGroupForm: createResourceGroupForm,
            createResourceGroupFormRules: createResourceGroupFormRules,
            handleResetCreateResourceGroupForm: handleResetCreateResourceGroupForm,
            handleCreateResourceGroupFormSubmit: handleCreateResourceGroupFormSubmit,
        };
    },
});
