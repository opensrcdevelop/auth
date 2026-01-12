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
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
import { reactive, ref } from "vue";
import { getResourceGroupList } from "@/api/resourceGroup";
import { handleApiSuccess } from "@/util/tool";
import { onMounted } from "vue";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
var props = withDefaults(defineProps(), {
    visible: false,
});
var emits = defineEmits();
/** 选择资源组表单 */
var selectResourceGroupFormRef = ref();
var selectResourceGroupForm = reactive({
    resourceGroupId: undefined,
});
var selectResourceGroupFormRules = {
    resourceGroupId: [{ required: true, message: "请选择资源组" }],
};
/** 资源组列表 */
var resourceGroupList = reactive([]);
var resourceGroupListPagination = {
    current: 1,
    total: 0,
};
var resourceGroupSearchKeyword = ref("");
/**
 * 获取资源组列表
 */
var handleGetResourceGroupList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getResourceGroupList({
        page: page,
        size: size,
        keyword: resourceGroupSearchKeyword.value,
    }).then(function (result) {
        handleApiSuccess(result, function (data) {
            if (resourceGroupListPagination.current === 1) {
                resourceGroupList.length = 0;
                resourceGroupList.push.apply(resourceGroupList, data.list);
            }
            else {
                resourceGroupList.push.apply(resourceGroupList, data.list);
            }
            resourceGroupListPagination.total = data.total;
        });
    });
};
var loadMoreResourceGroupLoading = false;
/** 加载更多资源组 */
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
/** 搜索资源组 */
var handleSearchResourceGroup = function () {
    handleGetResourceGroupList(1);
};
/** 关闭对话框 */
var handleCloseModal = function () {
    selectResourceGroupFormRef.value.resetFields();
    emits("close");
};
/**
 * 跳转到授权页面
 */
var handleToAuthorize = function () {
    var globalVariblesStore = useGlobalVariablesStore();
    var resourceGroup = resourceGroupList.find(function (item) { return item.id === selectResourceGroupForm.resourceGroupId; });
    globalVariblesStore.authorizeOptions.resourceGroup = resourceGroup;
    globalVariblesStore.saveData();
    handleCloseModal();
    router.push({
        path: "/permission/authorize",
    });
};
onMounted(function () {
    handleGetResourceGroupList();
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    visible: false,
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
var __VLS_0 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.visible), footer: (false), width: (680) })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.visible), footer: (false), width: (680) })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onCancel: (__VLS_ctx.handleCloseModal)
};
var __VLS_8 = {};
__VLS_3.slots.default;
{
    var __VLS_thisSlot = __VLS_3.slots.title;
}
var __VLS_9 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_10 = __VLS_asFunctionalComponent(__VLS_9, new __VLS_9(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.selectResourceGroupForm), rules: (__VLS_ctx.selectResourceGroupFormRules), ref: "selectResourceGroupFormRef" })));
var __VLS_11 = __VLS_10.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.selectResourceGroupForm), rules: (__VLS_ctx.selectResourceGroupFormRules), ref: "selectResourceGroupFormRef" })], __VLS_functionalComponentArgsRest(__VLS_10), false));
var __VLS_13;
var __VLS_14;
var __VLS_15;
var __VLS_16 = {
    onSubmitSuccess: (__VLS_ctx.handleToAuthorize)
};
/** @type {typeof __VLS_ctx.selectResourceGroupFormRef} */ ;
var __VLS_17 = {};
__VLS_12.slots.default;
var __VLS_19 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_20 = __VLS_asFunctionalComponent(__VLS_19, new __VLS_19({
    field: "resourceGroupId",
    label: "资源组",
}));
var __VLS_21 = __VLS_20.apply(void 0, __spreadArray([{
        field: "resourceGroupId",
        label: "资源组",
    }], __VLS_functionalComponentArgsRest(__VLS_20), false));
__VLS_22.slots.default;
var __VLS_23 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_24 = __VLS_asFunctionalComponent(__VLS_23, new __VLS_23(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择资源组", allowClear: true, allowSearch: true, modelValue: (__VLS_ctx.selectResourceGroupForm.resourceGroupId), inputValue: (__VLS_ctx.resourceGroupSearchKeyword), filterOption: (false) })));
var __VLS_25 = __VLS_24.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择资源组", allowClear: true, allowSearch: true, modelValue: (__VLS_ctx.selectResourceGroupForm.resourceGroupId), inputValue: (__VLS_ctx.resourceGroupSearchKeyword), filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_24), false));
var __VLS_27;
var __VLS_28;
var __VLS_29;
var __VLS_30 = {
    onSearch: (__VLS_ctx.handleSearchResourceGroup)
};
var __VLS_31 = {
    onClear: (__VLS_ctx.handleSearchResourceGroup)
};
var __VLS_32 = {
    onDropdownReachBottom: (__VLS_ctx.loadMoreResourceGroup)
};
__VLS_26.slots.default;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.resourceGroupList)); _i < _a.length; _i++) {
    var resourceGroup = _a[_i][0];
    var __VLS_33 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_34 = __VLS_asFunctionalComponent(__VLS_33, new __VLS_33({
        key: (resourceGroup.id),
        value: (resourceGroup.id),
    }));
    var __VLS_35 = __VLS_34.apply(void 0, __spreadArray([{
            key: (resourceGroup.id),
            value: (resourceGroup.id),
        }], __VLS_functionalComponentArgsRest(__VLS_34), false));
    __VLS_36.slots.default;
    (resourceGroup.name);
    (resourceGroup.code);
    var __VLS_36;
}
var __VLS_26;
var __VLS_22;
var __VLS_37 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_38 = __VLS_asFunctionalComponent(__VLS_37, new __VLS_37({
    hideLabel: true,
}));
var __VLS_39 = __VLS_38.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_38), false));
__VLS_40.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "btn-container" }));
var __VLS_41 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_42 = __VLS_asFunctionalComponent(__VLS_41, new __VLS_41({}));
var __VLS_43 = __VLS_42.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_42), false));
__VLS_44.slots.default;
var __VLS_45 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_46 = __VLS_asFunctionalComponent(__VLS_45, new __VLS_45(__assign({ 'onClick': {} })));
var __VLS_47 = __VLS_46.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_46), false));
var __VLS_49;
var __VLS_50;
var __VLS_51;
var __VLS_52 = {
    onClick: (__VLS_ctx.handleCloseModal)
};
__VLS_48.slots.default;
var __VLS_48;
var __VLS_53 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_54 = __VLS_asFunctionalComponent(__VLS_53, new __VLS_53({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_55 = __VLS_54.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_54), false));
__VLS_56.slots.default;
var __VLS_56;
var __VLS_44;
var __VLS_40;
var __VLS_12;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['btn-container']} */ ;
// @ts-ignore
var __VLS_18 = __VLS_17;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            selectResourceGroupFormRef: selectResourceGroupFormRef,
            selectResourceGroupForm: selectResourceGroupForm,
            selectResourceGroupFormRules: selectResourceGroupFormRules,
            resourceGroupList: resourceGroupList,
            resourceGroupSearchKeyword: resourceGroupSearchKeyword,
            loadMoreResourceGroup: loadMoreResourceGroup,
            handleSearchResourceGroup: handleSearchResourceGroup,
            handleCloseModal: handleCloseModal,
            handleToAuthorize: handleToAuthorize,
        };
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
; /* PartiallyEnd: #4569/main.vue */
