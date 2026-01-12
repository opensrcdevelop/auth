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
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { deleteDictData, getDictDataList, getDictDetail, updateDict, } from "@/api/dict";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { usePagination } from "@/hooks/usePagination";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("dict_info");
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
    handleTabInit(tabKey);
};
var handleTabInit = function (tabKey, id) {
    if (id === void 0) { id = dictId.value; }
    switch (tabKey) {
        case "dict_info":
            handleGetDictDetail(id);
            break;
        case "dict_data":
            handleGetDictDetail(id);
            handleGetDictDataList(id);
            break;
    }
};
/** 字典信息表单 */
var dictInfoFormRef = ref();
var dictInfoForm = reactive({
    id: undefined,
    name: undefined,
    code: undefined,
    desc: undefined,
});
var dictInfoFormRules = {
    name: [{ required: true, message: "字典名称未填写" }],
    code: [{ required: true, message: "字典标识未填写" }],
};
var dictId = ref("");
var dictName = ref("");
/**
 * 获取字典详情
 *
 * @param id 字典 ID
 */
var handleGetDictDetail = function (id) {
    getDictDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            dictId.value = data.id;
            dictName.value = data.name;
            dictInfoForm.id = data.id;
            dictInfoForm.name = data.name;
            dictInfoForm.code = data.code;
            dictInfoForm.desc = data.desc;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取字典详情");
    });
};
/**
 * 重置字典信息表单
 */
var handleResetDictInfoForm = function () {
    dictInfoFormRef.value.resetFields();
    handleGetDictDetail(dictId.value);
};
/**
 * 提交字典信息表单
 *
 * @param formData 字典信息表单
 */
var handleDictInfoFormSubmit = function (formData) {
    updateDict(formData)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            Notification.success("保存成功");
            handleGetDictDetail(dictId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新字典信息");
    });
};
/** 字典数据列表 */
var dictDataSerachKeyword = ref(null);
var dictDataList = reactive([]);
var dictDataPagination;
/**
 * 获取字典数据列表
 *
 * @param page 页数
 * @param size 条数
 */
var handleGetDictDataList = function (id, page, size) {
    if (id === void 0) { id = dictId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getDictDataList(id, {
        page: page,
        size: size,
        keyword: dictDataSerachKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            dictDataList.length = 0;
            dictDataList.push.apply(dictDataList, data.list);
            dictDataPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取字典数据列表");
    });
};
/**
 * 跳转字典数据详情
 *
 * @param dictData 字典数据
 */
var handleToDictDataDetail = function (dictData) {
    router.push({
        path: "/dict/data/detail",
        query: {
            id: dictData.id,
            active_tab: "dict_data_info",
        },
    });
};
/**
 * 跳转创建字典数据
 */
var handleToCreateDictData = function () {
    var globalVariables = useGlobalVariablesStore();
    globalVariables.dictId = dictId.value;
    globalVariables.saveData();
    router.push({
        path: "/dict/data/create",
    });
};
/**
 * 删除字典数据
 */
var handleDeleteDictData = function (dicData) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u5B57\u5178\u6570\u636E\u300C".concat(dicData.label, "\u300D\u5417\uFF1F"),
        content: "此操作将删除该字典数据关联的用户字段值，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteDictData(dicData.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetDictDataList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除字典数据");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        var dictId = getQueryString("id");
        var tab = getQueryString("active_tab");
        dictDataPagination = usePagination("".concat(dictId, "_dictDataList"), function (_a) {
            var page = _a.page, size = _a.size;
            if (tab === "dict_data") {
                handleGetDictDataList(dictId, page, size);
            }
        });
        onMounted(function () {
            activeTab.value = tab || "dict_info";
            handleTabInit(tab, dictId);
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            dictInfoFormRef: dictInfoFormRef,
            dictInfoForm: dictInfoForm,
            dictInfoFormRules: dictInfoFormRules,
            dictId: dictId,
            dictName: dictName,
            handleResetDictInfoForm: handleResetDictInfoForm,
            handleDictInfoFormSubmit: handleDictInfoFormSubmit,
            dictDataSerachKeyword: dictDataSerachKeyword,
            dictDataList: dictDataList,
            dictDataPagination: dictDataPagination,
            handleGetDictDataList: handleGetDictDataList,
            handleToDictDataDetail: handleToDictDataDetail,
            handleToCreateDictData: handleToCreateDictData,
            handleDeleteDictData: handleDeleteDictData,
        };
    },
});
