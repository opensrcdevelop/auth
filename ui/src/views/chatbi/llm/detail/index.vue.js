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
import indexTs from "./index";
export default indexTs;
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
(__VLS_ctx.modelProviderName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.modelProviderId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.modelProviderId),
        textColor: "#86909c",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
var __VLS_3;
var __VLS_12 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    activeKey: (__VLS_ctx.activeTab),
}));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{
        activeKey: (__VLS_ctx.activeTab),
    }], __VLS_functionalComponentArgsRest(__VLS_13), false));
__VLS_15.slots.default;
var __VLS_16 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    key: "model_provider_info",
    title: "模型提供商信息",
}));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([{
        key: "model_provider_info",
        title: "模型提供商信息",
    }], __VLS_functionalComponentArgsRest(__VLS_17), false));
__VLS_19.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_20 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.modelProviderInfoForm), layout: "vertical", ref: "modelProviderInfoFormRef", rules: (__VLS_ctx.modelProviderInfoFormRules) })));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.modelProviderInfoForm), layout: "vertical", ref: "modelProviderInfoFormRef", rules: (__VLS_ctx.modelProviderInfoFormRules) })], __VLS_functionalComponentArgsRest(__VLS_21), false));
var __VLS_24;
var __VLS_25;
var __VLS_26;
var __VLS_27 = {
    onSubmitSuccess: (__VLS_ctx.handleModelProviderInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.modelProviderInfoFormRef} */ ;
var __VLS_28 = {};
__VLS_23.slots.default;
var __VLS_30 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30({
    gutter: (24),
}));
var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_31), false));
__VLS_33.slots.default;
var __VLS_34 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
    span: (12),
}));
var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_35), false));
__VLS_37.slots.default;
var __VLS_38 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
    field: "name",
    label: "模型提供商名称",
}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
        field: "name",
        label: "模型提供商名称",
    }], __VLS_functionalComponentArgsRest(__VLS_39), false));
__VLS_41.slots.default;
var __VLS_42 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    modelValue: (__VLS_ctx.modelProviderInfoForm.name),
    placeholder: "请输入模型提供商名称",
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelProviderInfoForm.name),
        placeholder: "请输入模型提供商名称",
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
var __VLS_41;
var __VLS_37;
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
    field: "type",
    label: "模型提供商类型",
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        field: "type",
        label: "模型提供商类型",
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
__VLS_53.slots.default;
var __VLS_54 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
    modelValue: (__VLS_ctx.modelProviderInfoForm.type),
    disabled: true,
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelProviderInfoForm.type),
        disabled: true,
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
__VLS_57.slots.default;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.modelProviderTypeList)); _i < _a.length; _i++) {
    var item = _a[_i][0];
    var __VLS_58 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
        value: (item.value),
        label: (item.label),
    }));
    var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
            value: (item.value),
            label: (item.label),
        }], __VLS_functionalComponentArgsRest(__VLS_59), false));
}
var __VLS_57;
var __VLS_53;
var __VLS_49;
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
    field: "baseUrl",
    label: "API 接入地址",
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        field: "baseUrl",
        label: "API 接入地址",
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
__VLS_69.slots.default;
var __VLS_70 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
    modelValue: (__VLS_ctx.modelProviderInfoForm.baseUrl),
    placeholder: "请输入 API 接入地址",
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelProviderInfoForm.baseUrl),
        placeholder: "请输入 API 接入地址",
    }], __VLS_functionalComponentArgsRest(__VLS_71), false));
var __VLS_69;
var __VLS_65;
var __VLS_74 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    span: (12),
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
__VLS_77.slots.default;
var __VLS_78 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
    field: "apiKey",
    label: "API Key",
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        field: "apiKey",
        label: "API Key",
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_82 = {}.AInputPassword;
/** @type {[typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
    modelValue: (__VLS_ctx.modelProviderInfoForm.apiKey),
    placeholder: "请输入 API Key",
}));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelProviderInfoForm.apiKey),
        placeholder: "请输入 API Key",
    }], __VLS_functionalComponentArgsRest(__VLS_83), false));
var __VLS_81;
var __VLS_77;
var __VLS_86 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
    span: (12),
}));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_87), false));
__VLS_89.slots.default;
var __VLS_90 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
    field: "temperature",
    label: "温度参数",
}));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
        field: "temperature",
        label: "温度参数",
    }], __VLS_functionalComponentArgsRest(__VLS_91), false));
__VLS_93.slots.default;
var __VLS_94 = {}.AInputNumber;
/** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
    modelValue: (__VLS_ctx.modelProviderInfoForm.temperature),
    min: (0),
    hideButton: true,
    placeholder: "请输入温度参数",
}));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelProviderInfoForm.temperature),
        min: (0),
        hideButton: true,
        placeholder: "请输入温度参数",
    }], __VLS_functionalComponentArgsRest(__VLS_95), false));
var __VLS_93;
var __VLS_89;
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
    field: "maxTokens",
    label: "最大输出 token 长度",
}));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
        field: "maxTokens",
        label: "最大输出 token 长度",
    }], __VLS_functionalComponentArgsRest(__VLS_103), false));
__VLS_105.slots.default;
var __VLS_106 = {}.AInputNumber;
/** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
// @ts-ignore
var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({
    modelValue: (__VLS_ctx.modelProviderInfoForm.maxTokens),
    min: (50),
    hideButton: true,
    placeholder: "请输入最大输出 token 长度",
}));
var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelProviderInfoForm.maxTokens),
        min: (50),
        hideButton: true,
        placeholder: "请输入最大输出 token 长度",
    }], __VLS_functionalComponentArgsRest(__VLS_107), false));
var __VLS_105;
var __VLS_101;
var __VLS_110 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110({
    span: (12),
}));
var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_111), false));
__VLS_113.slots.default;
var __VLS_114 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
    field: "defaultModel",
    label: "默认模型",
}));
var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
        field: "defaultModel",
        label: "默认模型",
    }], __VLS_functionalComponentArgsRest(__VLS_115), false));
__VLS_117.slots.default;
var __VLS_118 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
    modelValue: (__VLS_ctx.modelProviderInfoForm.defaultModel),
}));
var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelProviderInfoForm.defaultModel),
    }], __VLS_functionalComponentArgsRest(__VLS_119), false));
__VLS_121.slots.default;
for (var _b = 0, _c = __VLS_getVForSourceType((__VLS_ctx.optionalModelList)); _b < _c.length; _b++) {
    var item = _c[_b][0];
    var __VLS_122 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
        value: (item.name),
        label: (item.name),
    }));
    var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
            value: (item.name),
            label: (item.name),
        }], __VLS_functionalComponentArgsRest(__VLS_123), false));
}
var __VLS_121;
var __VLS_117;
var __VLS_113;
var __VLS_33;
var __VLS_126 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126({
    hideLabel: true,
}));
var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_127), false));
__VLS_129.slots.default;
var __VLS_130 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130({}));
var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_131), false));
__VLS_133.slots.default;
var __VLS_134 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_135), false));
__VLS_137.slots.default;
var __VLS_137;
var __VLS_138 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_139 = __VLS_asFunctionalComponent(__VLS_138, new __VLS_138(__assign({ 'onClick': {} })));
var __VLS_140 = __VLS_139.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_139), false));
var __VLS_142;
var __VLS_143;
var __VLS_144;
var __VLS_145 = {
    onClick: (__VLS_ctx.handleResetModelProviderInfoForm)
};
__VLS_141.slots.default;
var __VLS_141;
var __VLS_133;
var __VLS_129;
var __VLS_23;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "add-model-btn" }));
var __VLS_146 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_147 = __VLS_asFunctionalComponent(__VLS_146, new __VLS_146(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
var __VLS_148 = __VLS_147.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_147), false));
var __VLS_150;
var __VLS_151;
var __VLS_152;
var __VLS_153 = {
    onClick: (function () { return (__VLS_ctx.addOptionalModelModalVisible = true); })
};
__VLS_149.slots.default;
{
    var __VLS_thisSlot = __VLS_149.slots.icon;
    var __VLS_154 = {}.IconPlusCircle;
    /** @type {[typeof __VLS_components.IconPlusCircle, typeof __VLS_components.iconPlusCircle, ]} */ ;
    // @ts-ignore
    var __VLS_155 = __VLS_asFunctionalComponent(__VLS_154, new __VLS_154({}));
    var __VLS_156 = __VLS_155.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_155), false));
}
{
    var __VLS_thisSlot = __VLS_149.slots.default;
}
var __VLS_149;
var __VLS_158 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_159 = __VLS_asFunctionalComponent(__VLS_158, new __VLS_158(__assign({ 'onChange': {} }, { data: (__VLS_ctx.optionalModelList), bordered: (false), pagination: (false), draggable: ({ type: 'handle', width: 40 }) })));
var __VLS_160 = __VLS_159.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { data: (__VLS_ctx.optionalModelList), bordered: (false), pagination: (false), draggable: ({ type: 'handle', width: 40 }) })], __VLS_functionalComponentArgsRest(__VLS_159), false));
var __VLS_162;
var __VLS_163;
var __VLS_164;
var __VLS_165 = {
    onChange: (__VLS_ctx.handleOptionalModelListChange)
};
__VLS_161.slots.default;
{
    var __VLS_thisSlot = __VLS_161.slots.columns;
    var __VLS_166 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_167 = __VLS_asFunctionalComponent(__VLS_166, new __VLS_166({
        title: "模型名称",
    }));
    var __VLS_168 = __VLS_167.apply(void 0, __spreadArray([{
            title: "模型名称",
        }], __VLS_functionalComponentArgsRest(__VLS_167), false));
    __VLS_169.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_169.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.name);
    }
    var __VLS_169;
    var __VLS_170 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_171 = __VLS_asFunctionalComponent(__VLS_170, new __VLS_170({
        title: "已使用输入 token 数",
    }));
    var __VLS_172 = __VLS_171.apply(void 0, __spreadArray([{
            title: "已使用输入 token 数",
        }], __VLS_functionalComponentArgsRest(__VLS_171), false));
    __VLS_173.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_173.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.usedReqTokens.toLocaleString());
    }
    var __VLS_173;
    var __VLS_174 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_175 = __VLS_asFunctionalComponent(__VLS_174, new __VLS_174({
        title: "已使用输出 token 数",
    }));
    var __VLS_176 = __VLS_175.apply(void 0, __spreadArray([{
            title: "已使用输出 token 数",
        }], __VLS_functionalComponentArgsRest(__VLS_175), false));
    __VLS_177.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_177.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.usedRepTokens.toLocaleString());
    }
    var __VLS_177;
    var __VLS_178 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_179 = __VLS_asFunctionalComponent(__VLS_178, new __VLS_178({
        title: "操作",
        width: (80),
    }));
    var __VLS_180 = __VLS_179.apply(void 0, __spreadArray([{
            title: "操作",
            width: (80),
        }], __VLS_functionalComponentArgsRest(__VLS_179), false));
    __VLS_181.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_181.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        if (record_1.name !== __VLS_ctx.modelProviderInfoForm.defaultModel) {
            var __VLS_182 = {}.AButton;
            /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
            // @ts-ignore
            var __VLS_183 = __VLS_asFunctionalComponent(__VLS_182, new __VLS_182(__assign({ 'onClick': {} }, { type: "text", status: "danger", size: "mini" })));
            var __VLS_184 = __VLS_183.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", status: "danger", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_183), false));
            var __VLS_186 = void 0;
            var __VLS_187 = void 0;
            var __VLS_188 = void 0;
            var __VLS_189 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    if (!(record_1.name !== __VLS_ctx.modelProviderInfoForm.defaultModel))
                        return;
                    __VLS_ctx.handleRemoveOptionalModel(record_1);
                }
            };
            __VLS_185.slots.default;
            {
                var __VLS_thisSlot_5 = __VLS_185.slots.icon;
                var __VLS_190 = {}.IconDelete;
                /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
                // @ts-ignore
                var __VLS_191 = __VLS_asFunctionalComponent(__VLS_190, new __VLS_190({}));
                var __VLS_192 = __VLS_191.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_191), false));
            }
            {
                var __VLS_thisSlot_6 = __VLS_185.slots.default;
            }
            var __VLS_185;
        }
    }
    var __VLS_181;
}
var __VLS_161;
var __VLS_19;
var __VLS_15;
var __VLS_194 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_195 = __VLS_asFunctionalComponent(__VLS_194, new __VLS_194(__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.addOptionalModelModalVisible) })));
var __VLS_196 = __VLS_195.apply(void 0, __spreadArray([__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.addOptionalModelModalVisible) })], __VLS_functionalComponentArgsRest(__VLS_195), false));
var __VLS_198;
var __VLS_199;
var __VLS_200;
var __VLS_201 = {
    onOk: (__VLS_ctx.handleAddOptionalModelFormSubmit)
};
var __VLS_202 = {
    onCancel: (__VLS_ctx.handleCloseAddOptionalModelModal)
};
__VLS_197.slots.default;
{
    var __VLS_thisSlot = __VLS_197.slots.title;
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
var __VLS_203 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_204 = __VLS_asFunctionalComponent(__VLS_203, new __VLS_203({
    model: (__VLS_ctx.addOptionalModelForm),
    ref: "addOptionalModelFormRef",
    rules: (__VLS_ctx.addOptionalModelFormRules),
}));
var __VLS_205 = __VLS_204.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.addOptionalModelForm),
        ref: "addOptionalModelFormRef",
        rules: (__VLS_ctx.addOptionalModelFormRules),
    }], __VLS_functionalComponentArgsRest(__VLS_204), false));
/** @type {typeof __VLS_ctx.addOptionalModelFormRef} */ ;
var __VLS_207 = {};
__VLS_206.slots.default;
var __VLS_209 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_210 = __VLS_asFunctionalComponent(__VLS_209, new __VLS_209({
    field: "name",
    hideLabel: true,
}));
var __VLS_211 = __VLS_210.apply(void 0, __spreadArray([{
        field: "name",
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_210), false));
__VLS_212.slots.default;
var __VLS_213 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_214 = __VLS_asFunctionalComponent(__VLS_213, new __VLS_213({
    modelValue: (__VLS_ctx.addOptionalModelForm.name),
    placeholder: "请输入模型名称",
}));
var __VLS_215 = __VLS_214.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.addOptionalModelForm.name),
        placeholder: "请输入模型名称",
    }], __VLS_functionalComponentArgsRest(__VLS_214), false));
var __VLS_212;
var __VLS_206;
var __VLS_197;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['add-model-btn']} */ ;
// @ts-ignore
var __VLS_29 = __VLS_28, __VLS_208 = __VLS_207;
var __VLS_dollars;
var __VLS_self;
