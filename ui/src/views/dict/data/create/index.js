import { createDictData } from "@/api/dict";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/** 创建字典数据表单 */
var createDictDataFormRef = ref();
var createDictDataForm = reactive({
    label: undefined,
    value: undefined,
    enable: true,
    displaySeq: undefined,
    dictId: undefined,
});
var createDictDataFormRules = {
    label: [{ required: true, message: "数据标签未填写" }],
    value: [{ required: true, message: "数据值未填写" }],
    enable: [{ required: true, message: "是否启用未选择" }],
};
/**
 * 提交创建字典数据表单
 */
var handleCreateDictDataFormSubmit = function (formData) {
    createDictData(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreateDictDataForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建字典数据");
    });
};
/**
 * 重置创建字典数据表单
 */
var handleResetCreateDictDataForm = function () {
    createDictDataFormRef.value.resetFields();
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            var globalVariables = useGlobalVariablesStore().getData();
            createDictDataForm.dictId = globalVariables.dictId;
        });
        return {
            handleBack: handleBack,
            createDictDataFormRef: createDictDataFormRef,
            createDictDataForm: createDictDataForm,
            createDictDataFormRules: createDictDataFormRules,
            handleCreateDictDataFormSubmit: handleCreateDictDataFormSubmit,
            handleResetCreateDictDataForm: handleResetCreateDictDataForm,
        };
    },
});
