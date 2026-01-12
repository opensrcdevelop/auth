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
(__VLS_ctx.userColumnName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.userColumnId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.userColumnId),
        textColor: "#86909c",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
var __VLS_12 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })], __VLS_functionalComponentArgsRest(__VLS_13), false));
var __VLS_16;
var __VLS_17;
var __VLS_18;
var __VLS_19 = {
    onChange: (__VLS_ctx.handleTabChange)
};
__VLS_15.slots.default;
var __VLS_20 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
    key: "user_column_info",
    title: "用户字段信息",
}));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{
        key: "user_column_info",
        title: "用户字段信息",
    }], __VLS_functionalComponentArgsRest(__VLS_21), false));
__VLS_23.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_24 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24(__assign({ 'onSubmit': {} }, { model: (__VLS_ctx.userColumnInfoForm), ref: "userColumnInfoFormRef", rules: (__VLS_ctx.userColumnInfoFormRules), layout: "vertical" })));
var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([__assign({ 'onSubmit': {} }, { model: (__VLS_ctx.userColumnInfoForm), ref: "userColumnInfoFormRef", rules: (__VLS_ctx.userColumnInfoFormRules), layout: "vertical" })], __VLS_functionalComponentArgsRest(__VLS_25), false));
var __VLS_28;
var __VLS_29;
var __VLS_30;
var __VLS_31 = {
    onSubmit: (__VLS_ctx.handleUserColumnInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.userColumnInfoFormRef} */ ;
var __VLS_32 = {};
__VLS_27.slots.default;
var __VLS_34 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
    gutter: (24),
}));
var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_35), false));
__VLS_37.slots.default;
var __VLS_38 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
    span: (12),
}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_39), false));
__VLS_41.slots.default;
var __VLS_42 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    field: "name",
    label: "字段名称",
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        field: "name",
        label: "字段名称",
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    modelValue: (__VLS_ctx.userColumnInfoForm.name),
    placeholder: "请输入字段名称",
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userColumnInfoForm.name),
        placeholder: "请输入字段名称",
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
var __VLS_45;
var __VLS_41;
var __VLS_50 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
    span: (12),
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
__VLS_53.slots.default;
var __VLS_54 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
    field: "key",
    label: "字段 Key",
    tooltip: "创建后不可以修改",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        field: "key",
        label: "字段 Key",
        tooltip: "创建后不可以修改",
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
__VLS_57.slots.default;
var __VLS_58 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
    modelValue: (__VLS_ctx.userColumnInfoForm.key),
    placeholder: "请输入字段 Key",
    disabled: true,
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userColumnInfoForm.key),
        placeholder: "请输入字段 Key",
        disabled: true,
    }], __VLS_functionalComponentArgsRest(__VLS_59), false));
var __VLS_57;
var __VLS_53;
var __VLS_62 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    span: (12),
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
__VLS_65.slots.default;
var __VLS_66 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
    field: "dataType",
    label: "表单类型",
    tooltip: "创建后不可以修改",
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        field: "dataType",
        label: "表单类型",
        tooltip: "创建后不可以修改",
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
__VLS_69.slots.default;
var __VLS_70 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
    modelValue: (__VLS_ctx.userColumnInfoForm.dataType),
    placeholder: "请选择表单类型",
    disabled: true,
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userColumnInfoForm.dataType),
        placeholder: "请选择表单类型",
        disabled: true,
    }], __VLS_functionalComponentArgsRest(__VLS_71), false));
__VLS_73.slots.default;
var __VLS_74 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    value: "STRING",
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        value: "STRING",
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
__VLS_77.slots.default;
var __VLS_77;
var __VLS_78 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
    value: "BOOLEAN",
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        value: "BOOLEAN",
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_81;
var __VLS_82 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
    value: "NUMBER",
}));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
        value: "NUMBER",
    }], __VLS_functionalComponentArgsRest(__VLS_83), false));
__VLS_85.slots.default;
var __VLS_85;
var __VLS_86 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
    value: "DATETIME",
}));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
        value: "DATETIME",
    }], __VLS_functionalComponentArgsRest(__VLS_87), false));
__VLS_89.slots.default;
var __VLS_89;
var __VLS_90 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
    value: "DATE",
}));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
        value: "DATE",
    }], __VLS_functionalComponentArgsRest(__VLS_91), false));
__VLS_93.slots.default;
var __VLS_93;
var __VLS_94 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
    value: "DICT",
}));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
        value: "DICT",
    }], __VLS_functionalComponentArgsRest(__VLS_95), false));
__VLS_97.slots.default;
var __VLS_97;
var __VLS_73;
var __VLS_69;
var __VLS_65;
var __VLS_98 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
    span: (12),
}));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_99), false));
__VLS_101.slots.default;
var __VLS_102 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
    field: "extFlg",
    label: "字段类型",
    tooltip: "创建后不可以修改",
}));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
        field: "extFlg",
        label: "字段类型",
        tooltip: "创建后不可以修改",
    }], __VLS_functionalComponentArgsRest(__VLS_103), false));
__VLS_105.slots.default;
var __VLS_106 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({
    modelValue: (__VLS_ctx.userColumnInfoForm.extFlg),
    placeholder: "请选择字段类型",
    disabled: true,
}));
var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userColumnInfoForm.extFlg),
        placeholder: "请选择字段类型",
        disabled: true,
    }], __VLS_functionalComponentArgsRest(__VLS_107), false));
__VLS_109.slots.default;
var __VLS_110 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110({
    value: (true),
}));
var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([{
        value: (true),
    }], __VLS_functionalComponentArgsRest(__VLS_111), false));
__VLS_113.slots.default;
var __VLS_113;
var __VLS_114 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
    value: (false),
}));
var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
        value: (false),
    }], __VLS_functionalComponentArgsRest(__VLS_115), false));
__VLS_117.slots.default;
var __VLS_117;
var __VLS_109;
var __VLS_105;
var __VLS_101;
var __VLS_118 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
    span: (12),
}));
var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_119), false));
__VLS_121.slots.default;
var __VLS_122 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
    field: "userLstDisplay",
    label: "是否在用户列表显示",
}));
var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
        field: "userLstDisplay",
        label: "是否在用户列表显示",
    }], __VLS_functionalComponentArgsRest(__VLS_123), false));
__VLS_125.slots.default;
var __VLS_126 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126({
    modelValue: (__VLS_ctx.userColumnInfoForm.userLstDisplay),
    placeholder: "请选择是否在用户列表显示",
}));
var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userColumnInfoForm.userLstDisplay),
        placeholder: "请选择是否在用户列表显示",
    }], __VLS_functionalComponentArgsRest(__VLS_127), false));
__VLS_129.slots.default;
var __VLS_130 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130({
    value: (true),
}));
var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{
        value: (true),
    }], __VLS_functionalComponentArgsRest(__VLS_131), false));
__VLS_133.slots.default;
var __VLS_133;
var __VLS_134 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134({
    value: (false),
}));
var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([{
        value: (false),
    }], __VLS_functionalComponentArgsRest(__VLS_135), false));
__VLS_137.slots.default;
var __VLS_137;
var __VLS_129;
var __VLS_125;
var __VLS_121;
var __VLS_138 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_139 = __VLS_asFunctionalComponent(__VLS_138, new __VLS_138({
    span: (12),
}));
var __VLS_140 = __VLS_139.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_139), false));
__VLS_141.slots.default;
var __VLS_142 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_143 = __VLS_asFunctionalComponent(__VLS_142, new __VLS_142({
    field: "displayWidth",
    label: "用户列表显示宽度",
}));
var __VLS_144 = __VLS_143.apply(void 0, __spreadArray([{
        field: "displayWidth",
        label: "用户列表显示宽度",
    }], __VLS_functionalComponentArgsRest(__VLS_143), false));
__VLS_145.slots.default;
var __VLS_146 = {}.AInputNumber;
/** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
// @ts-ignore
var __VLS_147 = __VLS_asFunctionalComponent(__VLS_146, new __VLS_146({
    modelValue: (__VLS_ctx.userColumnInfoForm.displayWidth),
    placeholder: "请输入用户列表显示宽度",
    hideButton: true,
    min: (10),
}));
var __VLS_148 = __VLS_147.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userColumnInfoForm.displayWidth),
        placeholder: "请输入用户列表显示宽度",
        hideButton: true,
        min: (10),
    }], __VLS_functionalComponentArgsRest(__VLS_147), false));
__VLS_149.slots.default;
{
    var __VLS_thisSlot = __VLS_149.slots.suffix;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
}
var __VLS_149;
var __VLS_145;
var __VLS_141;
var __VLS_150 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_151 = __VLS_asFunctionalComponent(__VLS_150, new __VLS_150({
    span: (12),
}));
var __VLS_152 = __VLS_151.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_151), false));
__VLS_153.slots.default;
var __VLS_154 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_155 = __VLS_asFunctionalComponent(__VLS_154, new __VLS_154({
    field: "userVisible",
    label: "个人中心是否可见",
}));
var __VLS_156 = __VLS_155.apply(void 0, __spreadArray([{
        field: "userVisible",
        label: "个人中心是否可见",
    }], __VLS_functionalComponentArgsRest(__VLS_155), false));
__VLS_157.slots.default;
var __VLS_158 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_159 = __VLS_asFunctionalComponent(__VLS_158, new __VLS_158({
    modelValue: (__VLS_ctx.userColumnInfoForm.userVisible),
    placeholder: "请选择个人中心是否可见",
}));
var __VLS_160 = __VLS_159.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userColumnInfoForm.userVisible),
        placeholder: "请选择个人中心是否可见",
    }], __VLS_functionalComponentArgsRest(__VLS_159), false));
__VLS_161.slots.default;
var __VLS_162 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_163 = __VLS_asFunctionalComponent(__VLS_162, new __VLS_162({
    value: (true),
}));
var __VLS_164 = __VLS_163.apply(void 0, __spreadArray([{
        value: (true),
    }], __VLS_functionalComponentArgsRest(__VLS_163), false));
__VLS_165.slots.default;
var __VLS_165;
var __VLS_166 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_167 = __VLS_asFunctionalComponent(__VLS_166, new __VLS_166({
    value: (false),
}));
var __VLS_168 = __VLS_167.apply(void 0, __spreadArray([{
        value: (false),
    }], __VLS_functionalComponentArgsRest(__VLS_167), false));
__VLS_169.slots.default;
var __VLS_169;
var __VLS_161;
var __VLS_157;
var __VLS_153;
var __VLS_170 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_171 = __VLS_asFunctionalComponent(__VLS_170, new __VLS_170({
    span: (12),
}));
var __VLS_172 = __VLS_171.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_171), false));
__VLS_173.slots.default;
var __VLS_174 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_175 = __VLS_asFunctionalComponent(__VLS_174, new __VLS_174({
    field: "userEditable",
    label: "用户是否可编辑",
}));
var __VLS_176 = __VLS_175.apply(void 0, __spreadArray([{
        field: "userEditable",
        label: "用户是否可编辑",
    }], __VLS_functionalComponentArgsRest(__VLS_175), false));
__VLS_177.slots.default;
var __VLS_178 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_179 = __VLS_asFunctionalComponent(__VLS_178, new __VLS_178({
    modelValue: (__VLS_ctx.userColumnInfoForm.userEditable),
    placeholder: "请选择用户是否可编辑",
}));
var __VLS_180 = __VLS_179.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userColumnInfoForm.userEditable),
        placeholder: "请选择用户是否可编辑",
    }], __VLS_functionalComponentArgsRest(__VLS_179), false));
__VLS_181.slots.default;
var __VLS_182 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_183 = __VLS_asFunctionalComponent(__VLS_182, new __VLS_182({
    value: (true),
}));
var __VLS_184 = __VLS_183.apply(void 0, __spreadArray([{
        value: (true),
    }], __VLS_functionalComponentArgsRest(__VLS_183), false));
__VLS_185.slots.default;
var __VLS_185;
var __VLS_186 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_187 = __VLS_asFunctionalComponent(__VLS_186, new __VLS_186({
    value: (false),
}));
var __VLS_188 = __VLS_187.apply(void 0, __spreadArray([{
        value: (false),
    }], __VLS_functionalComponentArgsRest(__VLS_187), false));
__VLS_189.slots.default;
var __VLS_189;
var __VLS_181;
var __VLS_177;
var __VLS_173;
if (__VLS_ctx.userColumnInfoForm.dataType === 'DICT') {
    var __VLS_190 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_191 = __VLS_asFunctionalComponent(__VLS_190, new __VLS_190({
        span: (12),
    }));
    var __VLS_192 = __VLS_191.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_191), false));
    __VLS_193.slots.default;
    var __VLS_194 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_195 = __VLS_asFunctionalComponent(__VLS_194, new __VLS_194({
        field: "dictId",
        label: "字典类型",
        tooltip: "创建后不可以修改",
    }));
    var __VLS_196 = __VLS_195.apply(void 0, __spreadArray([{
            field: "dictId",
            label: "字典类型",
            tooltip: "创建后不可以修改",
        }], __VLS_functionalComponentArgsRest(__VLS_195), false));
    __VLS_197.slots.default;
    var __VLS_198 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_199 = __VLS_asFunctionalComponent(__VLS_198, new __VLS_198({
        modelValue: (__VLS_ctx.userColumnInfoForm.dictId),
        placeholder: "请选择字典类型",
        disabled: true,
    }));
    var __VLS_200 = __VLS_199.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.userColumnInfoForm.dictId),
            placeholder: "请选择字典类型",
            disabled: true,
        }], __VLS_functionalComponentArgsRest(__VLS_199), false));
    __VLS_201.slots.default;
    for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.dictList)); _i < _a.length; _i++) {
        var dict = _a[_i][0];
        var __VLS_202 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_203 = __VLS_asFunctionalComponent(__VLS_202, new __VLS_202({
            value: (dict.id),
            key: (dict.id),
        }));
        var __VLS_204 = __VLS_203.apply(void 0, __spreadArray([{
                value: (dict.id),
                key: (dict.id),
            }], __VLS_functionalComponentArgsRest(__VLS_203), false));
        __VLS_205.slots.default;
        (dict.name);
        var __VLS_205;
    }
    var __VLS_201;
    var __VLS_197;
    var __VLS_193;
}
var __VLS_37;
var __VLS_206 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_207 = __VLS_asFunctionalComponent(__VLS_206, new __VLS_206({
    hideLabel: true,
}));
var __VLS_208 = __VLS_207.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_207), false));
__VLS_209.slots.default;
var __VLS_210 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_211 = __VLS_asFunctionalComponent(__VLS_210, new __VLS_210({}));
var __VLS_212 = __VLS_211.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_211), false));
__VLS_213.slots.default;
var __VLS_214 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_215 = __VLS_asFunctionalComponent(__VLS_214, new __VLS_214({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_216 = __VLS_215.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_215), false));
__VLS_217.slots.default;
var __VLS_217;
var __VLS_218 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_219 = __VLS_asFunctionalComponent(__VLS_218, new __VLS_218(__assign({ 'onClick': {} })));
var __VLS_220 = __VLS_219.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_219), false));
var __VLS_222;
var __VLS_223;
var __VLS_224;
var __VLS_225 = {
    onClick: (__VLS_ctx.handleResetUserColumnInfoForm)
};
__VLS_221.slots.default;
var __VLS_221;
var __VLS_213;
var __VLS_209;
var __VLS_27;
var __VLS_23;
var __VLS_15;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
// @ts-ignore
var __VLS_33 = __VLS_32;
var __VLS_dollars;
var __VLS_self;
