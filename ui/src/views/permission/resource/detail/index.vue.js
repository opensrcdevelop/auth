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
(__VLS_ctx.resourceName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.resourceId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.resourceId),
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
    onClick: (__VLS_ctx.handleToCreatePermission)
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
    key: "resource_info",
    title: "资源信息",
}));
var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
        key: "resource_info",
        title: "资源信息",
    }], __VLS_functionalComponentArgsRest(__VLS_29), false));
__VLS_31.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_32 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.resourceInfoForm), ref: "resourceInfoFormRef", rules: (__VLS_ctx.resourceInfoFormRuels), layout: "vertical" })));
var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.resourceInfoForm), ref: "resourceInfoFormRef", rules: (__VLS_ctx.resourceInfoFormRuels), layout: "vertical" })], __VLS_functionalComponentArgsRest(__VLS_33), false));
var __VLS_36;
var __VLS_37;
var __VLS_38;
var __VLS_39 = {
    onSubmitSuccess: (__VLS_ctx.handleResourceInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.resourceInfoFormRef} */ ;
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
    label: "资源名称",
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        field: "name",
        label: "资源名称",
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
__VLS_53.slots.default;
var __VLS_54 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
    modelValue: (__VLS_ctx.resourceInfoForm.name),
    placeholder: "请输入资源名称",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceInfoForm.name),
        placeholder: "请输入资源名称",
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
    field: "name",
    label: "资源标识",
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        field: "name",
        label: "资源标识",
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
__VLS_65.slots.default;
var __VLS_66 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
    modelValue: (__VLS_ctx.resourceInfoForm.code),
    placeholder: "请输入资源标识",
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceInfoForm.code),
        placeholder: "请输入资源标识",
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
var __VLS_65;
var __VLS_61;
var __VLS_45;
var __VLS_70 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
    field: "api",
    label: "资源路径",
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        field: "api",
        label: "资源路径",
    }], __VLS_functionalComponentArgsRest(__VLS_71), false));
__VLS_73.slots.default;
var __VLS_74 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    modelValue: (__VLS_ctx.resourceInfoForm.api),
    placeholder: "请输入资源路径（URL）",
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceInfoForm.api),
        placeholder: "请输入资源路径（URL）",
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
var __VLS_73;
var __VLS_78 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
    field: "desc",
    label: "资源描述",
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        field: "desc",
        label: "资源描述",
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_82 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
    modelValue: (__VLS_ctx.resourceInfoForm.desc),
    placeholder: "请输入资源描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceInfoForm.desc),
        placeholder: "请输入资源描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_83), false));
var __VLS_81;
var __VLS_86 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
    hideLabel: true,
}));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_87), false));
__VLS_89.slots.default;
var __VLS_90 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({}));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_91), false));
__VLS_93.slots.default;
var __VLS_94 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_95), false));
__VLS_97.slots.default;
var __VLS_97;
var __VLS_98 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98(__assign({ 'onClick': {} })));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_99), false));
var __VLS_102;
var __VLS_103;
var __VLS_104;
var __VLS_105 = {
    onClick: (__VLS_ctx.handleResetResourceInfoForm)
};
__VLS_101.slots.default;
var __VLS_101;
var __VLS_93;
var __VLS_89;
var __VLS_35;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_106 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({
    model: (__VLS_ctx.resourceGroupInfo),
    layout: "vertical",
}));
var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.resourceGroupInfo),
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_107), false));
__VLS_109.slots.default;
var __VLS_110 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110({
    gutter: (24),
}));
var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_111), false));
__VLS_113.slots.default;
var __VLS_114 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
    span: (8),
}));
var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
        span: (8),
    }], __VLS_functionalComponentArgsRest(__VLS_115), false));
__VLS_117.slots.default;
var __VLS_118 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
    label: "资源组ID",
}));
var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
        label: "资源组ID",
    }], __VLS_functionalComponentArgsRest(__VLS_119), false));
__VLS_121.slots.default;
var __VLS_122 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
    text: (__VLS_ctx.resourceGroupInfo.id),
}));
var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.resourceGroupInfo.id),
    }], __VLS_functionalComponentArgsRest(__VLS_123), false));
var __VLS_121;
var __VLS_117;
var __VLS_126 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126({
    span: (8),
}));
var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([{
        span: (8),
    }], __VLS_functionalComponentArgsRest(__VLS_127), false));
__VLS_129.slots.default;
var __VLS_130 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130({
    label: "资源组名称",
}));
var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{
        label: "资源组名称",
    }], __VLS_functionalComponentArgsRest(__VLS_131), false));
__VLS_133.slots.default;
var __VLS_134 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134({
    modelValue: (__VLS_ctx.resourceGroupInfo.name),
    readonly: true,
}));
var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceGroupInfo.name),
        readonly: true,
    }], __VLS_functionalComponentArgsRest(__VLS_135), false));
var __VLS_133;
var __VLS_129;
var __VLS_138 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_139 = __VLS_asFunctionalComponent(__VLS_138, new __VLS_138({
    span: (8),
}));
var __VLS_140 = __VLS_139.apply(void 0, __spreadArray([{
        span: (8),
    }], __VLS_functionalComponentArgsRest(__VLS_139), false));
__VLS_141.slots.default;
var __VLS_142 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_143 = __VLS_asFunctionalComponent(__VLS_142, new __VLS_142({
    label: "资源组标识",
}));
var __VLS_144 = __VLS_143.apply(void 0, __spreadArray([{
        label: "资源组标识",
    }], __VLS_functionalComponentArgsRest(__VLS_143), false));
__VLS_145.slots.default;
var __VLS_146 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_147 = __VLS_asFunctionalComponent(__VLS_146, new __VLS_146({
    modelValue: (__VLS_ctx.resourceGroupInfo.code),
    readonly: true,
}));
var __VLS_148 = __VLS_147.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceGroupInfo.code),
        readonly: true,
    }], __VLS_functionalComponentArgsRest(__VLS_147), false));
var __VLS_145;
var __VLS_141;
var __VLS_113;
var __VLS_109;
var __VLS_31;
var __VLS_150 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_151 = __VLS_asFunctionalComponent(__VLS_150, new __VLS_150({
    key: "permission_list",
    title: "权限列表",
}));
var __VLS_152 = __VLS_151.apply(void 0, __spreadArray([{
        key: "permission_list",
        title: "权限列表",
    }], __VLS_functionalComponentArgsRest(__VLS_151), false));
__VLS_153.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_154 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_155 = __VLS_asFunctionalComponent(__VLS_154, new __VLS_154(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入权限名称或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.permissionSearchKeyword) })));
var __VLS_156 = __VLS_155.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入权限名称或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.permissionSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_155), false));
var __VLS_158;
var __VLS_159;
var __VLS_160;
var __VLS_161 = {
    onSearch: (__VLS_ctx.handleSearchResourcePermissions)
};
var __VLS_162 = {
    onKeyup: (__VLS_ctx.handleSearchResourcePermissions)
};
var __VLS_163 = {
    onClear: (__VLS_ctx.handleSearchResourcePermissions)
};
var __VLS_157;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "permission-list" }));
var __VLS_164 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.permissions), bordered: (false), pagination: (__VLS_ctx.permissionsPagination.pagination), scroll: ({ y: '100%' }) })));
var __VLS_166 = __VLS_165.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.permissions), bordered: (false), pagination: (__VLS_ctx.permissionsPagination.pagination), scroll: ({ y: '100%' }) })], __VLS_functionalComponentArgsRest(__VLS_165), false));
var __VLS_168;
var __VLS_169;
var __VLS_170;
var __VLS_171 = {
    onPageChange: (__VLS_ctx.permissionsPagination.handlePageChange)
};
var __VLS_172 = {
    onPageSizeChange: (__VLS_ctx.permissionsPagination.handlePageSizeChange)
};
__VLS_167.slots.default;
{
    var __VLS_thisSlot = __VLS_167.slots.columns;
    var __VLS_173 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_174 = __VLS_asFunctionalComponent(__VLS_173, new __VLS_173({
        title: "权限名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_175 = __VLS_174.apply(void 0, __spreadArray([{
            title: "权限名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_174), false));
    __VLS_176.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_176.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToPermissionDetail(record_1);
            } }, { class: "table-column-permissionname" }));
        (record_1.permissionName);
    }
    var __VLS_176;
    var __VLS_177 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_178 = __VLS_asFunctionalComponent(__VLS_177, new __VLS_177({
        title: "权限标识",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_179 = __VLS_178.apply(void 0, __spreadArray([{
            title: "权限标识",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_178), false));
    __VLS_180.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_180.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        (record.permissionCode);
    }
    var __VLS_180;
    var __VLS_181 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_182 = __VLS_asFunctionalComponent(__VLS_181, new __VLS_181({
        title: "操作",
        width: (60),
    }));
    var __VLS_183 = __VLS_182.apply(void 0, __spreadArray([{
            title: "操作",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_182), false));
    __VLS_184.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_184.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        var __VLS_185 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_186 = __VLS_asFunctionalComponent(__VLS_185, new __VLS_185({}));
        var __VLS_187 = __VLS_186.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_186), false));
        __VLS_188.slots.default;
        var __VLS_189 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_190 = __VLS_asFunctionalComponent(__VLS_189, new __VLS_189({
            type: "text",
        }));
        var __VLS_191 = __VLS_190.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_190), false));
        __VLS_192.slots.default;
        {
            var __VLS_thisSlot_4 = __VLS_192.slots.icon;
            var __VLS_193 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_194 = __VLS_asFunctionalComponent(__VLS_193, new __VLS_193({}));
            var __VLS_195 = __VLS_194.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_194), false));
        }
        var __VLS_192;
        {
            var __VLS_thisSlot_5 = __VLS_188.slots.content;
            var __VLS_197 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_198 = __VLS_asFunctionalComponent(__VLS_197, new __VLS_197(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_199 = __VLS_198.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_198), false));
            var __VLS_201 = void 0;
            var __VLS_202 = void 0;
            var __VLS_203 = void 0;
            var __VLS_204 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleDeletePermission(record_2);
                }
            };
            __VLS_200.slots.default;
            {
                var __VLS_thisSlot_6 = __VLS_200.slots.icon;
                var __VLS_205 = {}.IconDelete;
                /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
                // @ts-ignore
                var __VLS_206 = __VLS_asFunctionalComponent(__VLS_205, new __VLS_205({}));
                var __VLS_207 = __VLS_206.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_206), false));
            }
            var __VLS_200;
        }
        var __VLS_188;
    }
    var __VLS_184;
}
var __VLS_167;
var __VLS_153;
var __VLS_23;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['permission-list']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-permissionname']} */ ;
// @ts-ignore
var __VLS_41 = __VLS_40;
var __VLS_dollars;
var __VLS_self;
