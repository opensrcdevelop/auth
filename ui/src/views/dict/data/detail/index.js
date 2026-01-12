var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
import { getDictDataDetail, updateDictData } from "@/api/dict";
import router from "@/router";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("dict_data_info");
/**
 * tab 切换事件
 *
 * @param tabKey tabKey
 */
var handleTabChange = function (tabKey) {
    router.replace({
        query: __assign(__assign({}, router.currentRoute.value.query), { active_tab: tabKey }),
    });
    activeTab.value = tabKey;
};
/** 字典数据信息表单 */
var dictDataInfoFormRef = ref();
var dictDataInfoForm = reactive({
    dictId: undefined,
    id: undefined,
    label: undefined,
    value: undefined,
    enable: undefined,
    displaySeq: undefined,
});
var dictDataInfoFormRules = {
    label: [{ required: true, message: "数据标签未填写" }],
    value: [{ required: true, message: "数据值未填写" }],
};
var dictDataId = ref("");
var dictDataLabel = ref("");
/**
 * 获取字典数据详情
 *
 * @param id 字典数据 ID
 */
var handleGetDictDatatDetail = function (id) {
    getDictDataDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            dictDataId.value = data.id;
            dictDataLabel.value = data.label;
            dictDataInfoForm.dictId = data.dictId;
            dictDataInfoForm.id = data.id;
            dictDataInfoForm.label = data.label;
            dictDataInfoForm.value = data.value;
            dictDataInfoForm.enable = data.enable;
            dictDataInfoForm.displaySeq = data.displaySeq;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取字典数据详情");
    });
};
/**
 * 重置字典数据信息表单
 */
var handleResetDictDataInfoForm = function () {
    dictDataInfoFormRef.value.resetFields();
    handleGetDictDatatDetail(dictDataId.value);
};
/**
 * 提交字典数据信息表单
 *
 * @param formData 字典数据信息表单
 */
var handleDictDataInfoFormSubmit = function (formData) {
    updateDictData(formData)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            Notification.success("保存成功");
            handleGetDictDatatDetail(dictDataId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新字典数据信息");
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "dict_data_info";
            handleGetDictDatatDetail(getQueryString("id"));
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            dictDataInfoFormRef: dictDataInfoFormRef,
            dictDataInfoForm: dictDataInfoForm,
            dictDataInfoFormRules: dictDataInfoFormRules,
            dictDataId: dictDataId,
            dictDataLabel: dictDataLabel,
            handleDictDataInfoFormSubmit: handleDictDataInfoFormSubmit,
            handleResetDictDataInfoForm: handleResetDictDataInfoForm
        };
    },
});
