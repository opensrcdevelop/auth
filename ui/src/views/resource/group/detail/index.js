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
import { getGroupResources, getResourceGroupDetail, updateResourceGroup, } from "@/api/resourceGroup";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { usePagination } from "@/hooks/usePagination";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("resource_group_info");
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
    if (id === void 0) { id = resourceGroupId.value; }
    switch (tabKey) {
        case "resource_group_info":
            handleGetResourceGroupDetail(id);
            break;
        case "resource_list":
            handleGetResourceGroupDetail(id);
            handleGetGroupResourceList(id);
            break;
    }
};
var resourceGroupId = ref("");
var resourceGroupName = ref("");
/** 资源组信息表单 */
var resourceGroupInfoFromRef = ref();
var resourceGroupInfoFrom = reactive({
    id: undefined,
    name: undefined,
    code: undefined,
    desc: undefined,
});
var resourceGroupInfoFormRules = {
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
};
/**
 * 获取资源组详情
 *
 * @param id
 */
var handleGetResourceGroupDetail = function (id) {
    getResourceGroupDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            resourceGroupId.value = data.id;
            resourceGroupName.value = data.name;
            resourceGroupInfoFrom.id = data.id;
            resourceGroupInfoFrom.name = data.name;
            resourceGroupInfoFrom.code = data.code;
            resourceGroupInfoFrom.desc = data.desc;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取资源组详情");
    });
};
/** 组内资源列表 */
var resourceList = reactive([]);
var resourceSearchKeyword = ref("");
var resoueceListPagination;
/**
 * 获取组内资源列表
 *
 * @param id 资源组ID
 * @param page 页数
 * @param size 条数
 */
var handleGetGroupResourceList = function (id, page, size) {
    if (id === void 0) { id = resourceGroupId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getGroupResources(id, {
        page: page,
        size: size,
        keyword: resourceSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            resourceList.length = 0;
            resourceList.push.apply(resourceList, data.list);
            resoueceListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取组内资源列表");
    });
};
/**
 * 提交资源组信息表单
 */
var handleResourceGroupInfoFormSubmit = function (formData) {
    updateResourceGroup(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetResourceGroupDetail(resourceGroupId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新资源组");
    });
};
/**
 * 重置资源组信息表单
 */
var handleResetResourceGroupInfoForm = function () {
    resourceGroupInfoFromRef.value.resetFields();
    handleGetResourceGroupDetail(resourceGroupId.value);
};
/**
 * 搜索组内资源
 */
var handleSearchResource = function () {
    handleGetGroupResourceList();
};
/**
 * 跳转资源详情
 */
var handleToResourceDetail = function (resource) {
    router.push({
        path: "/permission/resource/detail/",
        query: {
            id: resource.id,
            active_tab: "resource_info",
        },
    });
};
/**
 * 跳转创建资源
 */
var handleToCreateResource = function () {
    var globalVariables = useGlobalVariablesStore();
    globalVariables.resourceGroupId = resourceGroupId.value;
    globalVariables.saveData();
    router.push({
        path: "/permission/resource/create/",
    });
};
export default defineComponent({
    setup: function () {
        var resourceGroupId = getQueryString("id");
        resoueceListPagination = usePagination("".concat(resourceGroupId, "_resourceList"), function (_a) {
            var page = _a.page, size = _a.size;
            if (getQueryString("active_tab") === "resource_list") {
                handleGetGroupResourceList(resourceGroupId, page, size);
            }
        });
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "resource_group_info";
            handleTabInit(activeTab.value, resourceGroupId);
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            resourceGroupId: resourceGroupId,
            resourceGroupName: resourceGroupName,
            resourceGroupInfoFromRef: resourceGroupInfoFromRef,
            resourceGroupInfoFrom: resourceGroupInfoFrom,
            resourceGroupInfoFormRules: resourceGroupInfoFormRules,
            handleResourceGroupInfoFormSubmit: handleResourceGroupInfoFormSubmit,
            handleResetResourceGroupInfoForm: handleResetResourceGroupInfoForm,
            resourceSearchKeyword: resourceSearchKeyword,
            resourceList: resourceList,
            resoueceListPagination: resoueceListPagination,
            handleSearchResource: handleSearchResource,
            handleToResourceDetail: handleToResourceDetail,
            handleToCreateResource: handleToCreateResource,
        };
    },
});
