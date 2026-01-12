import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { createResource } from "@/api/resource";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { getResourceGroupList } from "@/api/resourceGroup";
import { useGlobalVariablesStore } from "@/store/globalVariables";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/** 资源组列表 */
var resourceGroupList = reactive([]);
var resourceGroupListPagination = {
    current: 1,
    total: 0,
};
var resourceGroupSearchKeyword = ref("");
var handleGetResourceGroupList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getResourceGroupList({
        page: page,
        size: size,
        keyword: resourceGroupSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page === 1) {
                resourceGroupList.length = 0;
                resourceGroupList.push.apply(resourceGroupList, data.list);
            }
            else {
                resourceGroupList.push.apply(resourceGroupList, data.list);
            }
            resourceGroupListPagination.current = data.current;
            resourceGroupListPagination.total = data.total;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取资源组列表");
    });
};
/** 显示选择资源组 */
var showSelectResourceGroup = ref(true);
/**
 * 搜索资源组
 */
var handleSearchResourceGroup = function () {
    handleGetResourceGroupList(1);
};
var loadMoreResourceGroupLoading = false;
/**
 * 加载更多资源组
 */
var loadMoreResourceGroup = function () {
    if (loadMoreResourceGroupLoading)
        return;
    if (resourceGroupList.length < resourceGroupListPagination.total) {
        loadMoreResourceGroupLoading = true;
        resourceGroupListPagination.current++;
        handleGetResourceGroupList(resourceGroupListPagination.current);
        loadMoreResourceGroupLoading = false;
    }
};
/** 创建资源表单 */
var createResourceFormRef = ref();
var createResourceForm = reactive({
    name: undefined,
    code: undefined,
    desc: undefined,
    api: undefined,
    resourceGroupId: undefined,
});
var createResourceFormRules = {
    name: [{ required: true, message: "资源名称未填写" }],
    code: [
        { required: true, message: "资源标识未填写" },
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
    resourceGroupId: [{ required: true, message: "资源组未选择" }]
};
/**
 * 提交创建资源表单
 */
var handleCreateResourceFormSubmit = function (formData) {
    createResource(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreateResourceForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建资源");
    });
};
/**
 * 重置创建资源表单
 */
var handleResetCreateResourceForm = function () {
    createResourceFormRef.value.resetFields();
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            var globalVariables = useGlobalVariablesStore().getData();
            if (!globalVariables.resourceGroupId) {
                handleGetResourceGroupList();
                createResourceForm.resourceGroupId = undefined;
                showSelectResourceGroup.value = true;
            }
            else {
                createResourceForm.resourceGroupId = globalVariables.resourceGroupId;
                showSelectResourceGroup.value = false;
            }
        });
        return {
            handleBack: handleBack,
            createResourceFormRef: createResourceFormRef,
            createResourceForm: createResourceForm,
            createResourceFormRules: createResourceFormRules,
            handleCreateResourceFormSubmit: handleCreateResourceFormSubmit,
            handleResetCreateResourceForm: handleResetCreateResourceForm,
            resourceGroupList: resourceGroupList,
            handleSearchResourceGroup: handleSearchResourceGroup,
            resourceGroupSearchKeyword: resourceGroupSearchKeyword,
            loadMoreResourceGroup: loadMoreResourceGroup,
            showSelectResourceGroup: showSelectResourceGroup
        };
    },
});
