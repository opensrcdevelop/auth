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
(__VLS_ctx.permissionName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.permissionId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.permissionId),
        textColor: "#86909c",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "locator" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_12 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    text: (__VLS_ctx.permissionLocator),
    textColor: "#86909c",
}));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.permissionLocator),
        textColor: "#86909c",
    }], __VLS_functionalComponentArgsRest(__VLS_13), false));
var __VLS_16 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16(__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })], __VLS_functionalComponentArgsRest(__VLS_17), false));
var __VLS_20;
var __VLS_21;
var __VLS_22;
var __VLS_23 = {
    onChange: (__VLS_ctx.handleTabChange)
};
__VLS_19.slots.default;
var __VLS_24 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
    key: "permission_info",
    title: "权限信息",
}));
var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([{
        key: "permission_info",
        title: "权限信息",
    }], __VLS_functionalComponentArgsRest(__VLS_25), false));
__VLS_27.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_28 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.permissionInfoForm), ref: "permissionInfoFormRef", rules: (__VLS_ctx.permissionInfoFormRules), layout: "vertical" })));
var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.permissionInfoForm), ref: "permissionInfoFormRef", rules: (__VLS_ctx.permissionInfoFormRules), layout: "vertical" })], __VLS_functionalComponentArgsRest(__VLS_29), false));
var __VLS_32;
var __VLS_33;
var __VLS_34;
var __VLS_35 = {
    onSubmitSuccess: (__VLS_ctx.permissionInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.permissionInfoFormRef} */ ;
var __VLS_36 = {};
__VLS_31.slots.default;
var __VLS_38 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
    gutter: (24),
}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_39), false));
__VLS_41.slots.default;
var __VLS_42 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    span: (12),
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    field: "name",
    label: "权限名称",
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        field: "name",
        label: "权限名称",
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
__VLS_49.slots.default;
var __VLS_50 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
    modelValue: (__VLS_ctx.permissionInfoForm.name),
    placeholder: "请输入资源名称",
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.permissionInfoForm.name),
        placeholder: "请输入资源名称",
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
var __VLS_49;
var __VLS_45;
var __VLS_54 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
    span: (12),
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
__VLS_57.slots.default;
var __VLS_58 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
    field: "code",
    label: "权限标识",
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        field: "code",
        label: "权限标识",
    }], __VLS_functionalComponentArgsRest(__VLS_59), false));
__VLS_61.slots.default;
var __VLS_62 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    modelValue: (__VLS_ctx.permissionInfoForm.code),
    placeholder: "请输入资源标识",
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.permissionInfoForm.code),
        placeholder: "请输入资源标识",
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
var __VLS_61;
var __VLS_57;
var __VLS_41;
var __VLS_66 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
    field: "desc",
    label: "权限描述",
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        field: "desc",
        label: "权限描述",
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
__VLS_69.slots.default;
var __VLS_70 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
    modelValue: (__VLS_ctx.permissionInfoForm.desc),
    placeholder: "请输入资源描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.permissionInfoForm.desc),
        placeholder: "请输入资源描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_71), false));
var __VLS_69;
var __VLS_74 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    hideLabel: true,
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
__VLS_77.slots.default;
var __VLS_78 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_82 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_83), false));
__VLS_85.slots.default;
var __VLS_85;
var __VLS_86 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86(__assign({ 'onClick': {} })));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_87), false));
var __VLS_90;
var __VLS_91;
var __VLS_92;
var __VLS_93 = {
    onClick: (__VLS_ctx.handleResetPermissionInfoForm)
};
__VLS_89.slots.default;
var __VLS_89;
var __VLS_81;
var __VLS_77;
var __VLS_31;
var __VLS_27;
var __VLS_94 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
    key: "authorization_management",
    title: "授权管理",
}));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
        key: "authorization_management",
        title: "授权管理",
    }], __VLS_functionalComponentArgsRest(__VLS_95), false));
__VLS_97.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_98 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "搜索主体", allowClear: true, modelValue: (__VLS_ctx.authorizedPrincipalSearchKeyword) })));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "搜索主体", allowClear: true, modelValue: (__VLS_ctx.authorizedPrincipalSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_99), false));
var __VLS_102;
var __VLS_103;
var __VLS_104;
var __VLS_105 = {
    onSearch: (__VLS_ctx.handleSerachAuthorizedPrincipal)
};
var __VLS_106 = {
    onKeyup: (__VLS_ctx.handleSerachAuthorizedPrincipal)
};
var __VLS_107 = {
    onClear: (__VLS_ctx.handleSerachAuthorizedPrincipal)
};
var __VLS_101;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "record-list" }));
var __VLS_108 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
    data: (__VLS_ctx.authorizeRecords),
    bordered: (false),
    pagination: (false),
    expandable: ({ width: 30 }),
    rowKey: "authorizeId",
}));
var __VLS_110 = __VLS_109.apply(void 0, __spreadArray([{
        data: (__VLS_ctx.authorizeRecords),
        bordered: (false),
        pagination: (false),
        expandable: ({ width: 30 }),
        rowKey: "authorizeId",
    }], __VLS_functionalComponentArgsRest(__VLS_109), false));
__VLS_111.slots.default;
{
    var __VLS_thisSlot = __VLS_111.slots.columns;
    var __VLS_112 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({
        title: "被授权主体",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_114 = __VLS_113.apply(void 0, __spreadArray([{
            title: "被授权主体",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_113), false));
    __VLS_115.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_115.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handeToPrincipalDetail(record_1);
            } }, { class: "table-column-name" }));
        (record_1.principal);
    }
    var __VLS_115;
    var __VLS_116 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
        title: "主体类型",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_118 = __VLS_117.apply(void 0, __spreadArray([{
            title: "主体类型",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_117), false));
    __VLS_119.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_119.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        (record.principalTypeDisplayName);
    }
    var __VLS_119;
    var __VLS_120 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
        title: "优先级",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
        width: (220),
    }));
    var __VLS_122 = __VLS_121.apply(void 0, __spreadArray([{
            title: "优先级",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
            width: (220),
        }], __VLS_functionalComponentArgsRest(__VLS_121), false));
    __VLS_123.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_123.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        var __VLS_124 = {}.AInputGroup;
        /** @type {[typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, ]} */ ;
        // @ts-ignore
        var __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({}));
        var __VLS_126 = __VLS_125.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_125), false));
        __VLS_127.slots.default;
        var __VLS_128 = {}.ASelect;
        /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
        // @ts-ignore
        var __VLS_129 = __VLS_asFunctionalComponent(__VLS_128, new __VLS_128(__assign({ placeholder: "请选择优先级", defaultValue: (0), modelValue: (record_2.priority) }, { style: {} })));
        var __VLS_130 = __VLS_129.apply(void 0, __spreadArray([__assign({ placeholder: "请选择优先级", defaultValue: (0), modelValue: (record_2.priority) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_129), false));
        __VLS_131.slots.default;
        var __VLS_132 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
            value: (-1),
        }));
        var __VLS_134 = __VLS_133.apply(void 0, __spreadArray([{
                value: (-1),
            }], __VLS_functionalComponentArgsRest(__VLS_133), false));
        __VLS_135.slots.default;
        var __VLS_135;
        var __VLS_136 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
            value: (0),
        }));
        var __VLS_138 = __VLS_137.apply(void 0, __spreadArray([{
                value: (0),
            }], __VLS_functionalComponentArgsRest(__VLS_137), false));
        __VLS_139.slots.default;
        var __VLS_139;
        var __VLS_140 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
            value: (1),
        }));
        var __VLS_142 = __VLS_141.apply(void 0, __spreadArray([{
                value: (1),
            }], __VLS_functionalComponentArgsRest(__VLS_141), false));
        __VLS_143.slots.default;
        var __VLS_143;
        var __VLS_144 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
            value: (2),
        }));
        var __VLS_146 = __VLS_145.apply(void 0, __spreadArray([{
                value: (2),
            }], __VLS_functionalComponentArgsRest(__VLS_145), false));
        __VLS_147.slots.default;
        var __VLS_147;
        var __VLS_148 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
            value: (3),
        }));
        var __VLS_150 = __VLS_149.apply(void 0, __spreadArray([{
                value: (3),
            }], __VLS_functionalComponentArgsRest(__VLS_149), false));
        __VLS_151.slots.default;
        var __VLS_151;
        var __VLS_131;
        var __VLS_152 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152(__assign({ 'onClick': {} })));
        var __VLS_154 = __VLS_153.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_153), false));
        var __VLS_156 = void 0;
        var __VLS_157 = void 0;
        var __VLS_158 = void 0;
        var __VLS_159 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleUpdateAuthorizePriority(record_2);
            }
        };
        __VLS_155.slots.default;
        var __VLS_155;
        var __VLS_127;
    }
    var __VLS_123;
    var __VLS_160 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({
        title: "授权时间",
        width: (180),
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_162 = __VLS_161.apply(void 0, __spreadArray([{
            title: "授权时间",
            width: (180),
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_161), false));
    __VLS_163.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_163.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        (record.authorizeTime);
    }
    var __VLS_163;
    var __VLS_164 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
        title: "操作",
        width: (60),
    }));
    var __VLS_166 = __VLS_165.apply(void 0, __spreadArray([{
            title: "操作",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_165), false));
    __VLS_167.slots.default;
    {
        var __VLS_thisSlot_5 = __VLS_167.slots.cell;
        var record_3 = __VLS_getSlotParams(__VLS_thisSlot_5)[0].record;
        var __VLS_168 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({}));
        var __VLS_170 = __VLS_169.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_169), false));
        __VLS_171.slots.default;
        var __VLS_172 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172({
            type: "text",
        }));
        var __VLS_174 = __VLS_173.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_173), false));
        __VLS_175.slots.default;
        {
            var __VLS_thisSlot_6 = __VLS_175.slots.icon;
            var __VLS_176 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176({}));
            var __VLS_178 = __VLS_177.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_177), false));
        }
        var __VLS_175;
        {
            var __VLS_thisSlot_7 = __VLS_171.slots.content;
            var __VLS_180 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_182 = __VLS_181.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_181), false));
            var __VLS_184 = void 0;
            var __VLS_185 = void 0;
            var __VLS_186 = void 0;
            var __VLS_187 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleCancelAuthorization(record_3.principalId);
                }
            };
            __VLS_183.slots.default;
            {
                var __VLS_thisSlot_8 = __VLS_183.slots.icon;
                var __VLS_188 = {}.IconUndo;
                /** @type {[typeof __VLS_components.IconUndo, typeof __VLS_components.iconUndo, ]} */ ;
                // @ts-ignore
                var __VLS_189 = __VLS_asFunctionalComponent(__VLS_188, new __VLS_188({}));
                var __VLS_190 = __VLS_189.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_189), false));
            }
            var __VLS_183;
        }
        var __VLS_171;
    }
    var __VLS_167;
}
{
    var __VLS_thisSlot = __VLS_111.slots["expand-row"];
    var record_4 = __VLS_getSlotParams(__VLS_thisSlot)[0].record;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "condition-container" }));
    if (record_4.conditions.length > 0) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    }
    else {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    }
    if (record_4.conditions.length > 0) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
        var _loop_1 = function (condition, index) {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "expression-container" }));
            var __VLS_192 = {}.ADescriptions;
            /** @type {[typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, ]} */ ;
            // @ts-ignore
            var __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192(__assign({ style: {} }, { key: (index), column: (1), bordered: true })));
            var __VLS_194 = __VLS_193.apply(void 0, __spreadArray([__assign({ style: {} }, { key: (index), column: (1), bordered: true })], __VLS_functionalComponentArgsRest(__VLS_193), false));
            __VLS_195.slots.default;
            var __VLS_196 = {}.ADescriptionsItem;
            /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
            // @ts-ignore
            var __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196({
                label: "限制条件名称",
            }));
            var __VLS_198 = __VLS_197.apply(void 0, __spreadArray([{
                    label: "限制条件名称",
                }], __VLS_functionalComponentArgsRest(__VLS_197), false));
            __VLS_199.slots.default;
            var __VLS_200 = {}.ALink;
            /** @type {[typeof __VLS_components.ALink, typeof __VLS_components.aLink, typeof __VLS_components.ALink, typeof __VLS_components.aLink, ]} */ ;
            // @ts-ignore
            var __VLS_201 = __VLS_asFunctionalComponent(__VLS_200, new __VLS_200({}));
            var __VLS_202 = __VLS_201.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_201), false));
            __VLS_203.slots.default;
            __VLS_asFunctionalElement(__VLS_intrinsicElements.a, __VLS_intrinsicElements.a)({
                href: ("/ui/permission/expression/detail?id=".concat(condition.id)),
                target: "_blank",
            });
            (condition.name);
            var __VLS_204 = {}.IconLaunch;
            /** @type {[typeof __VLS_components.IconLaunch, typeof __VLS_components.iconLaunch, ]} */ ;
            // @ts-ignore
            var __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204(__assign({ style: {} })));
            var __VLS_206 = __VLS_205.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_205), false));
            var __VLS_208 = {}.ADescriptionsItem;
            /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
            // @ts-ignore
            var __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
                label: "限制条件描述",
            }));
            var __VLS_210 = __VLS_209.apply(void 0, __spreadArray([{
                    label: "限制条件描述",
                }], __VLS_functionalComponentArgsRest(__VLS_209), false));
            __VLS_211.slots.default;
            (condition.desc ? condition.desc : "-");
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    if (!(record_4.conditions.length > 0))
                        return;
                    __VLS_ctx.handleRemoveAuthorizeCondition(record_4, condition);
                } }, { class: "expression-remove" }));
            var __VLS_212 = {}.IconMinusCircleFill;
            /** @type {[typeof __VLS_components.IconMinusCircleFill, typeof __VLS_components.iconMinusCircleFill, ]} */ ;
            // @ts-ignore
            var __VLS_213 = __VLS_asFunctionalComponent(__VLS_212, new __VLS_212({}));
            var __VLS_214 = __VLS_213.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_213), false));
        };
        var __VLS_203, __VLS_199, __VLS_211, __VLS_195;
        for (var _i = 0, _a = __VLS_getVForSourceType((record_4.conditions)); _i < _a.length; _i++) {
            var _b = _a[_i], condition = _b[0], index = _b[1];
            _loop_1(condition, index);
        }
    }
    var __VLS_216 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_217 = __VLS_asFunctionalComponent(__VLS_216, new __VLS_216(__assign(__assign({ 'onClick': {} }, { type: "text" }), { style: {} })));
    var __VLS_218 = __VLS_217.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { type: "text" }), { style: {} })], __VLS_functionalComponentArgsRest(__VLS_217), false));
    var __VLS_220 = void 0;
    var __VLS_221 = void 0;
    var __VLS_222 = void 0;
    var __VLS_223 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleOpenAddAuthorizeConditionModal(record_4);
        }
    };
    __VLS_219.slots.default;
    {
        var __VLS_thisSlot_9 = __VLS_219.slots.icon;
        var __VLS_224 = {}.IconPlus;
        /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
        // @ts-ignore
        var __VLS_225 = __VLS_asFunctionalComponent(__VLS_224, new __VLS_224({}));
        var __VLS_226 = __VLS_225.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_225), false));
    }
    var __VLS_219;
}
{
    var __VLS_thisSlot = __VLS_111.slots["expand-icon"];
    var expanded = __VLS_getSlotParams(__VLS_thisSlot)[0].expanded;
    if (!expanded) {
        var __VLS_228 = {}.IconRight;
        /** @type {[typeof __VLS_components.IconRight, typeof __VLS_components.iconRight, ]} */ ;
        // @ts-ignore
        var __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228({}));
        var __VLS_230 = __VLS_229.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_229), false));
    }
    if (expanded) {
        var __VLS_232 = {}.IconDown;
        /** @type {[typeof __VLS_components.IconDown, typeof __VLS_components.iconDown, ]} */ ;
        // @ts-ignore
        var __VLS_233 = __VLS_asFunctionalComponent(__VLS_232, new __VLS_232({}));
        var __VLS_234 = __VLS_233.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_233), false));
    }
}
var __VLS_111;
var __VLS_97;
var __VLS_19;
var __VLS_3;
var __VLS_236 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_237 = __VLS_asFunctionalComponent(__VLS_236, new __VLS_236(__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.addAuthorizeConditionModalVisible), footer: (false) })));
var __VLS_238 = __VLS_237.apply(void 0, __spreadArray([__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.addAuthorizeConditionModalVisible), footer: (false) })], __VLS_functionalComponentArgsRest(__VLS_237), false));
var __VLS_240;
var __VLS_241;
var __VLS_242;
var __VLS_243 = {
    onCancel: (__VLS_ctx.handleCloseAddAuthorizeConditionModal)
};
__VLS_239.slots.default;
{
    var __VLS_thisSlot = __VLS_239.slots.title;
}
var __VLS_244 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.addAuthorizeConditionForm), ref: "addAuthorizeConditionFormRef", rules: (__VLS_ctx.addAuthorizeConditionFormRules) })));
var __VLS_246 = __VLS_245.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.addAuthorizeConditionForm), ref: "addAuthorizeConditionFormRef", rules: (__VLS_ctx.addAuthorizeConditionFormRules) })], __VLS_functionalComponentArgsRest(__VLS_245), false));
var __VLS_248;
var __VLS_249;
var __VLS_250;
var __VLS_251 = {
    onSubmitSuccess: (__VLS_ctx.handleAddAuthorizeConditionFormSubmit)
};
/** @type {typeof __VLS_ctx.addAuthorizeConditionFormRef} */ ;
var __VLS_252 = {};
__VLS_247.slots.default;
var __VLS_254 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_255 = __VLS_asFunctionalComponent(__VLS_254, new __VLS_254({
    field: "permissionExpIds",
    label: "限制条件",
}));
var __VLS_256 = __VLS_255.apply(void 0, __spreadArray([{
        field: "permissionExpIds",
        label: "限制条件",
    }], __VLS_functionalComponentArgsRest(__VLS_255), false));
__VLS_257.slots.default;
var __VLS_258 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_259 = __VLS_asFunctionalComponent(__VLS_258, new __VLS_258(__assign(__assign({ 'onSearch': {} }, { 'onDropdownReachBottom': {} }), { modelValue: (__VLS_ctx.addAuthorizeConditionForm.permissionExpIds), inputValue: (__VLS_ctx.authorizeConditionSearchKeyword), multiple: true, allowClear: true, allowSearch: true, placeholder: "请选择限制条件", filterOption: (false) })));
var __VLS_260 = __VLS_259.apply(void 0, __spreadArray([__assign(__assign({ 'onSearch': {} }, { 'onDropdownReachBottom': {} }), { modelValue: (__VLS_ctx.addAuthorizeConditionForm.permissionExpIds), inputValue: (__VLS_ctx.authorizeConditionSearchKeyword), multiple: true, allowClear: true, allowSearch: true, placeholder: "请选择限制条件", filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_259), false));
var __VLS_262;
var __VLS_263;
var __VLS_264;
var __VLS_265 = {
    onSearch: (__VLS_ctx.handleSearchAuthorizeCondition)
};
var __VLS_266 = {
    onDropdownReachBottom: (__VLS_ctx.loadMoreAuthorizeCondition)
};
__VLS_261.slots.default;
for (var _c = 0, _d = __VLS_getVForSourceType((__VLS_ctx.authorizeConditionList)); _c < _d.length; _c++) {
    var _e = _d[_c], condition = _e[0], index = _e[1];
    var __VLS_267 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_268 = __VLS_asFunctionalComponent(__VLS_267, new __VLS_267({
        key: (condition.id),
        value: (condition.id),
    }));
    var __VLS_269 = __VLS_268.apply(void 0, __spreadArray([{
            key: (condition.id),
            value: (condition.id),
        }], __VLS_functionalComponentArgsRest(__VLS_268), false));
    __VLS_270.slots.default;
    (condition.name);
    var __VLS_270;
}
var __VLS_261;
var __VLS_257;
var __VLS_271 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_272 = __VLS_asFunctionalComponent(__VLS_271, new __VLS_271({}));
var __VLS_273 = __VLS_272.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_272), false));
__VLS_274.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "add-btn-container" }));
var __VLS_275 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_276 = __VLS_asFunctionalComponent(__VLS_275, new __VLS_275({}));
var __VLS_277 = __VLS_276.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_276), false));
__VLS_278.slots.default;
var __VLS_279 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_280 = __VLS_asFunctionalComponent(__VLS_279, new __VLS_279(__assign({ 'onClick': {} })));
var __VLS_281 = __VLS_280.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_280), false));
var __VLS_283;
var __VLS_284;
var __VLS_285;
var __VLS_286 = {
    onClick: (__VLS_ctx.handleCloseAddAuthorizeConditionModal)
};
__VLS_282.slots.default;
var __VLS_282;
var __VLS_287 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_288 = __VLS_asFunctionalComponent(__VLS_287, new __VLS_287({
    type: "primary",
    htmlType: "submit",
    loading: (__VLS_ctx.addAuthorizeConditionFormSubmitLoading),
}));
var __VLS_289 = __VLS_288.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
        loading: (__VLS_ctx.addAuthorizeConditionFormSubmitLoading),
    }], __VLS_functionalComponentArgsRest(__VLS_288), false));
__VLS_290.slots.default;
var __VLS_290;
var __VLS_278;
var __VLS_274;
var __VLS_247;
var __VLS_239;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['locator']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['record-list']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
/** @type {__VLS_StyleScopedClasses['condition-container']} */ ;
/** @type {__VLS_StyleScopedClasses['expression-container']} */ ;
/** @type {__VLS_StyleScopedClasses['expression-remove']} */ ;
/** @type {__VLS_StyleScopedClasses['add-btn-container']} */ ;
// @ts-ignore
var __VLS_37 = __VLS_36, __VLS_253 = __VLS_252;
var __VLS_dollars;
var __VLS_self;
