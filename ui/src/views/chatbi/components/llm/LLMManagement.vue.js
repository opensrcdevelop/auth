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
import { deleteModelProvider, getModelProviderList, updateModelProvider, } from "@/api/chatbi";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { reactive, ref } from "vue";
/** 模型提供商列表 */
var modelProviderList = reactive([]);
var modelProviderSearchKeyword = ref(null);
var modelProviderListPagination = usePagination("modelProviderList", function (_a) {
    var page = _a.page, size = _a.size;
    handleGetModelProviderList(page, size);
});
/**
 * 获取模型提供商列表
 */
var handleGetModelProviderList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getModelProviderList({
        page: page,
        size: size,
        keyword: modelProviderSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            modelProviderList.length = 0;
            modelProviderList.push.apply(modelProviderList, data.list);
            modelProviderListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取模型提供商列表");
    });
};
/**
 * 更新数据源启用状态
 */
var handleUpdateModelProviderState = function (modelProvider) {
    updateModelProvider({
        id: modelProvider.id,
        enabled: modelProvider.enabled,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("更新模型提供商启用状态成功");
            handleGetModelProviderList();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新模型提供商启用状态");
    });
};
/**
 * 跳转到模型提供商详情
 */
var handleToModelProviderDetail = function (modelProvider) {
    router.push({
        path: "/chatbi/llm/detail",
        query: {
            id: modelProvider.id,
            active_tab: "model_provider_info",
        },
    });
};
/**
 * 跳转到创建模型提供商
 */
var handleToCreateModelProvider = function () {
    router.push({
        path: "/chatbi/llm/create",
    });
};
/**
 * 删除模型提供商
 */
var handleDeleteModelProvider = function (modelProvider) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u6A21\u578B\u63D0\u4F9B\u5546\u300C".concat(modelProvider.name, "\u300D\u5417\uFF1F"),
        content: "此操作将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteModelProvider(modelProvider.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetModelProviderList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除模型提供商");
            });
        },
    });
};
/**
 * 初始化
 */
var init = function () {
    handleGetModelProviderList(modelProviderListPagination.pagination.current, modelProviderListPagination.pagination.pageSize);
};
var __VLS_exposed = {
    init: init,
};
defineExpose(__VLS_exposed);
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "header" }));
var __VLS_0 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入模型提供商名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.modelProviderSearchKeyword) })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入模型提供商名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.modelProviderSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetModelProviderList(1, 15);
    }
};
var __VLS_8 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetModelProviderList(1, 15);
    }
};
var __VLS_9 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetModelProviderList(1, 15);
    }
};
var __VLS_3;
var __VLS_10 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_11 = __VLS_asFunctionalComponent(__VLS_10, new __VLS_10(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_12 = __VLS_11.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_11), false));
var __VLS_14;
var __VLS_15;
var __VLS_16;
var __VLS_17 = {
    onClick: (__VLS_ctx.handleToCreateModelProvider)
};
__VLS_13.slots.default;
var __VLS_13;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "model-provider-list" }));
var __VLS_18 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.modelProviderList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.modelProviderListPagination.pagination) })));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.modelProviderList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.modelProviderListPagination.pagination) })], __VLS_functionalComponentArgsRest(__VLS_19), false));
var __VLS_22;
var __VLS_23;
var __VLS_24;
var __VLS_25 = {
    onPageChange: (__VLS_ctx.modelProviderListPagination.handlePageChange)
};
var __VLS_26 = {
    onPageSizeChange: (__VLS_ctx.modelProviderListPagination.handlePageSizeChange)
};
__VLS_21.slots.default;
{
    var __VLS_thisSlot = __VLS_21.slots.columns;
    var __VLS_27 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_28 = __VLS_asFunctionalComponent(__VLS_27, new __VLS_27({
        title: "模型提供商名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_29 = __VLS_28.apply(void 0, __spreadArray([{
            title: "模型提供商名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_28), false));
    __VLS_30.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_30.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToModelProviderDetail(record_1);
            } }, { class: "table-column-name" }));
        (record_1.name);
    }
    var __VLS_30;
    var __VLS_31 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_32 = __VLS_asFunctionalComponent(__VLS_31, new __VLS_31({
        title: "模型提供商类型",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_33 = __VLS_32.apply(void 0, __spreadArray([{
            title: "模型提供商类型",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_32), false));
    __VLS_34.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_34.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.type);
    }
    var __VLS_34;
    var __VLS_35 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_36 = __VLS_asFunctionalComponent(__VLS_35, new __VLS_35({
        title: "是否启用",
    }));
    var __VLS_37 = __VLS_36.apply(void 0, __spreadArray([{
            title: "是否启用",
        }], __VLS_functionalComponentArgsRest(__VLS_36), false));
    __VLS_38.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_38.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        var __VLS_39 = {}.ASwitch;
        /** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
        // @ts-ignore
        var __VLS_40 = __VLS_asFunctionalComponent(__VLS_39, new __VLS_39(__assign({ 'onChange': {} }, { type: "round", size: "small", modelValue: (record_2.enabled) })));
        var __VLS_41 = __VLS_40.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { type: "round", size: "small", modelValue: (record_2.enabled) })], __VLS_functionalComponentArgsRest(__VLS_40), false));
        var __VLS_43 = void 0;
        var __VLS_44 = void 0;
        var __VLS_45 = void 0;
        var __VLS_46 = {
            onChange: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleUpdateModelProviderState(record_2);
            }
        };
        var __VLS_42;
    }
    var __VLS_38;
    var __VLS_47 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_48 = __VLS_asFunctionalComponent(__VLS_47, new __VLS_47({
        title: "操作",
        fixed: "right",
        width: (60),
    }));
    var __VLS_49 = __VLS_48.apply(void 0, __spreadArray([{
            title: "操作",
            fixed: "right",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_48), false));
    __VLS_50.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_50.slots.cell;
        var record_3 = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        var __VLS_51 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_52 = __VLS_asFunctionalComponent(__VLS_51, new __VLS_51({}));
        var __VLS_53 = __VLS_52.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_52), false));
        __VLS_54.slots.default;
        var __VLS_55 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_56 = __VLS_asFunctionalComponent(__VLS_55, new __VLS_55({
            type: "text",
        }));
        var __VLS_57 = __VLS_56.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_56), false));
        __VLS_58.slots.default;
        {
            var __VLS_thisSlot_5 = __VLS_58.slots.icon;
            var __VLS_59 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_60 = __VLS_asFunctionalComponent(__VLS_59, new __VLS_59({}));
            var __VLS_61 = __VLS_60.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_60), false));
        }
        var __VLS_58;
        {
            var __VLS_thisSlot_6 = __VLS_54.slots.content;
            var __VLS_63 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_64 = __VLS_asFunctionalComponent(__VLS_63, new __VLS_63(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_65 = __VLS_64.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_64), false));
            var __VLS_67 = void 0;
            var __VLS_68 = void 0;
            var __VLS_69 = void 0;
            var __VLS_70 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleDeleteModelProvider(record_3);
                }
            };
            __VLS_66.slots.default;
            {
                var __VLS_thisSlot_7 = __VLS_66.slots.icon;
                var __VLS_71 = {}.IconDelete;
                /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
                // @ts-ignore
                var __VLS_72 = __VLS_asFunctionalComponent(__VLS_71, new __VLS_71({}));
                var __VLS_73 = __VLS_72.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_72), false));
            }
            var __VLS_66;
        }
        var __VLS_54;
    }
    var __VLS_50;
}
var __VLS_21;
/** @type {__VLS_StyleScopedClasses['header']} */ ;
/** @type {__VLS_StyleScopedClasses['model-provider-list']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            modelProviderList: modelProviderList,
            modelProviderSearchKeyword: modelProviderSearchKeyword,
            modelProviderListPagination: modelProviderListPagination,
            handleGetModelProviderList: handleGetModelProviderList,
            handleUpdateModelProviderState: handleUpdateModelProviderState,
            handleToModelProviderDetail: handleToModelProviderDetail,
            handleToCreateModelProvider: handleToCreateModelProvider,
            handleDeleteModelProvider: handleDeleteModelProvider,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return __assign({}, __VLS_exposed);
    },
});
; /* PartiallyEnd: #4569/main.vue */
