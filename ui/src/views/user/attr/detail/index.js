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
import { getUserAttrDetail, updateUserAttr } from "@/api/user";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { getDictList } from "@/api/dict";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("user_column_info");
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
var userColumnId = ref("");
var userColumnName = ref("");
/** 用户字段信息表单 */
var userColumnInfoFormRef = ref();
var userColumnInfoForm = reactive({
    id: undefined,
    name: undefined,
    key: undefined,
    dataType: undefined,
    extFlg: undefined,
    userLstDisplay: undefined,
    displayWidth: undefined,
    userVisible: undefined,
    userEditable: undefined,
    dictId: undefined,
});
var userColumnInfoFormRules = {
    name: [{ required: true, message: "字段名称未填写" }],
    userLstDisplay: [{ required: true, message: "是否在用户列表显示未选择" }],
    userVisible: [{ required: true, message: "个人中心是否可见未选择" }],
    userEditable: [{ required: true, message: "用户是否可编辑未选择" }],
};
/**
 * 获取用户字段详情
 *
 * @param id 用户字段id
 */
var handleGetUserColumnDetail = function (id) {
    getUserAttrDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userColumnId.value = data.id;
            userColumnName.value = data.name;
            userColumnInfoForm.id = data.id;
            userColumnInfoForm.name = data.name;
            userColumnInfoForm.key = data.key;
            userColumnInfoForm.dataType = data.dataType;
            userColumnInfoForm.extFlg = data.extFlg;
            userColumnInfoForm.userLstDisplay = data.userLstDisplay;
            userColumnInfoForm.displayWidth = data.displayWidth;
            userColumnInfoForm.userVisible = data.userVisible;
            userColumnInfoForm.userEditable = data.userEditable;
            userColumnInfoForm.dictId = data.dictId;
            if (data.dataType === "DICT" && data.dictId) {
                handleGetDicttList();
            }
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户字段详情");
    });
};
/** 字典列表 */
var dictList = reactive([]);
var handleGetDicttList = function () {
    getDictList({
        size: -1,
        page: 15,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            dictList.length = 0;
            dictList.push.apply(dictList, data.list);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取字典列表");
    });
};
/**
 * 重置用户字段信息表单
 */
var handleResetUserColumnInfoForm = function () {
    userColumnInfoFormRef.value.resetFields();
    handleGetUserColumnDetail(userColumnId.value);
};
/**
 * 提交用户字段信息表单
 */
var handleUserColumnInfoFormSubmit = function () {
    updateUserAttr(userColumnInfoForm)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetUserColumnDetail(userColumnId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新用户字段");
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "user_column_info";
            handleGetUserColumnDetail(getQueryString("id"));
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            userColumnId: userColumnId,
            userColumnName: userColumnName,
            userColumnInfoFormRef: userColumnInfoFormRef,
            userColumnInfoForm: userColumnInfoForm,
            userColumnInfoFormRules: userColumnInfoFormRules,
            handleResetUserColumnInfoForm: handleResetUserColumnInfoForm,
            handleUserColumnInfoFormSubmit: handleUserColumnInfoFormSubmit,
            dictList: dictList
        };
    },
});
