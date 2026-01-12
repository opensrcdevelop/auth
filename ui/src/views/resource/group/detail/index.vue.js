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
import detailTs from "./index";
export default detailTs;
debugger; /* PartiallyEnd: #3632/script.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
var __VLS_0 = {}.PageHeader;
/** @type {[typeof __VLS_components.PageHeader, typeof __VLS_components.pageHeader, typeof __VLS_components.PageHeader, typeof __VLS_components.pageHeader, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onBack': {} })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onBack': {} })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onBack: (__VLS_ctx.handleBack)
};
__VLS_3.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "detail-header" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "title" }));
(__VLS_ctx.resourceGroupName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.resourceGroupId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.resourceGroupId),
        textColor: "#86909c",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
var __VLS_12 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_13), false));
var __VLS_16;
var __VLS_17;
var __VLS_18;
var __VLS_19 = {
    onClick: (__VLS_ctx.handleToCreateResource)
};
__VLS_15.slots.default;
var __VLS_15;
var __VLS_20 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20(__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })], __VLS_functionalComponentArgsRest(__VLS_21), false));
var __VLS_24;
var __VLS_25;
var __VLS_26;
var __VLS_27 = {
    onChange: (__VLS_ctx.handleTabChange)
};
__VLS_23.slots.default;
var __VLS_28 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    key: "resource_group_info",
    title: "资源组信息",
}));
var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
        key: "resource_group_info",
        title: "资源组信息",
    }], __VLS_functionalComponentArgsRest(__VLS_29), false));
__VLS_31.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_32 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.resourceGroupInfoFrom), ref: "resourceGroupInfoFromRef", rules: (__VLS_ctx.resourceGroupInfoFormRules), layout: "vertical" })));
var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.resourceGroupInfoFrom), ref: "resourceGroupInfoFromRef", rules: (__VLS_ctx.resourceGroupInfoFormRules), layout: "vertical" })], __VLS_functionalComponentArgsRest(__VLS_33), false));
var __VLS_36;
var __VLS_37;
var __VLS_38;
var __VLS_39 = {
    onSubmitSuccess: (__VLS_ctx.handleResourceGroupInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.resourceGroupInfoFromRef} */ ;
var __VLS_40 = {};
__VLS_35.slots.default;
var __VLS_42 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    gutter: (24),
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    span: (12),
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
__VLS_49.slots.default;
var __VLS_50 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
    field: "name",
    label: "资源组名称",
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        field: "name",
        label: "资源组名称",
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
__VLS_53.slots.default;
var __VLS_54 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
    modelValue: (__VLS_ctx.resourceGroupInfoFrom.name),
    placeholder: "请输入资源组名称",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceGroupInfoFrom.name),
        placeholder: "请输入资源组名称",
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
var __VLS_53;
var __VLS_49;
var __VLS_58 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
    span: (12),
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_59), false));
__VLS_61.slots.default;
var __VLS_62 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    field: "code",
    label: "资源组标识",
    tooltip: "创建后不可以修改",
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        field: "code",
        label: "资源组标识",
        tooltip: "创建后不可以修改",
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
__VLS_65.slots.default;
var __VLS_66 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
    modelValue: (__VLS_ctx.resourceGroupInfoFrom.code),
    disabled: true,
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceGroupInfoFrom.code),
        disabled: true,
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
var __VLS_65;
var __VLS_61;
var __VLS_45;
var __VLS_70 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
    field: "desc",
    label: "资源组描述",
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        field: "desc",
        label: "资源组描述",
    }], __VLS_functionalComponentArgsRest(__VLS_71), false));
__VLS_73.slots.default;
var __VLS_74 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    modelValue: (__VLS_ctx.resourceGroupInfoFrom.desc),
    placeholder: "请输入资源组描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceGroupInfoFrom.desc),
        placeholder: "请输入资源组描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
var __VLS_73;
var __VLS_78 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
    hideLabel: true,
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_82 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({}));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_83), false));
__VLS_85.slots.default;
var __VLS_86 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_87), false));
__VLS_89.slots.default;
var __VLS_89;
var __VLS_90 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90(__assign({ 'onClick': {} })));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_91), false));
var __VLS_94;
var __VLS_95;
var __VLS_96;
var __VLS_97 = {
    onClick: (__VLS_ctx.handleResetResourceGroupInfoForm)
};
__VLS_93.slots.default;
var __VLS_93;
var __VLS_85;
var __VLS_81;
var __VLS_35;
var __VLS_31;
var __VLS_98 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
    key: "resource_list",
    title: "资源列表",
}));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
        key: "resource_list",
        title: "资源列表",
    }], __VLS_functionalComponentArgsRest(__VLS_99), false));
__VLS_101.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_102 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onKeyup': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入资源名称或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.resourceSearchKeyword) })));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onKeyup': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入资源名称或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.resourceSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_103), false));
var __VLS_106;
var __VLS_107;
var __VLS_108;
var __VLS_109 = {
    onSearch: (__VLS_ctx.handleSearchResource)
};
var __VLS_110 = {
    onClear: (__VLS_ctx.handleSearchResource)
};
var __VLS_111 = {
    onKeyup: (__VLS_ctx.handleSearchResource)
};
var __VLS_105;
var __VLS_112 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112(__assign(__assign(__assign({ 'onPageSizeChange': {} }, { 'onPageChange': {} }), { class: "resource-list" }), { data: (__VLS_ctx.resourceList), pagination: (__VLS_ctx.resoueceListPagination.pagination), bordered: (false), scroll: ({ y: '100%' }) })));
var __VLS_114 = __VLS_113.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onPageSizeChange': {} }, { 'onPageChange': {} }), { class: "resource-list" }), { data: (__VLS_ctx.resourceList), pagination: (__VLS_ctx.resoueceListPagination.pagination), bordered: (false), scroll: ({ y: '100%' }) })], __VLS_functionalComponentArgsRest(__VLS_113), false));
var __VLS_116;
var __VLS_117;
var __VLS_118;
var __VLS_119 = {
    onPageSizeChange: (__VLS_ctx.resoueceListPagination.handlePageSizeChange)
};
var __VLS_120 = {
    onPageChange: (__VLS_ctx.resoueceListPagination.handlePageChange)
};
__VLS_115.slots.default;
{
    var __VLS_thisSlot = __VLS_115.slots.columns;
    var __VLS_121 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_122 = __VLS_asFunctionalComponent(__VLS_121, new __VLS_121({
        title: "资源名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_123 = __VLS_122.apply(void 0, __spreadArray([{
            title: "资源名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_122), false));
    __VLS_124.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_124.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToResourceDetail(record_1);
            } }, { class: "table-column-name" }));
        (record_1.name);
    }
    var __VLS_124;
    var __VLS_125 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_126 = __VLS_asFunctionalComponent(__VLS_125, new __VLS_125({
        title: "资源标识",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_127 = __VLS_126.apply(void 0, __spreadArray([{
            title: "资源标识",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_126), false));
    __VLS_128.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_128.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.code);
    }
    var __VLS_128;
    var __VLS_129 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_130 = __VLS_asFunctionalComponent(__VLS_129, new __VLS_129({
        title: "资源路径",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_131 = __VLS_130.apply(void 0, __spreadArray([{
            title: "资源路径",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_130), false));
    __VLS_132.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_132.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.api);
    }
    var __VLS_132;
}
var __VLS_115;
var __VLS_101;
var __VLS_23;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['resource-list']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
// @ts-ignore
var __VLS_41 = __VLS_40;
var __VLS_dollars;
var __VLS_self;
