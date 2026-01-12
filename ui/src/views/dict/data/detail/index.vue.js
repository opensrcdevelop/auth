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
(__VLS_ctx.dictDataLabel);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.dictDataId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.dictDataId),
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
    key: "dict_data_info",
    title: "字典数据信息",
}));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{
        key: "dict_data_info",
        title: "字典数据信息",
    }], __VLS_functionalComponentArgsRest(__VLS_21), false));
__VLS_23.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_24 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.dictDataInfoForm), ref: "dictDataInfoFormRef", rules: (__VLS_ctx.dictDataInfoFormRules), layout: "vertical" })));
var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.dictDataInfoForm), ref: "dictDataInfoFormRef", rules: (__VLS_ctx.dictDataInfoFormRules), layout: "vertical" })], __VLS_functionalComponentArgsRest(__VLS_25), false));
var __VLS_28;
var __VLS_29;
var __VLS_30;
var __VLS_31 = {
    onSubmitSuccess: (__VLS_ctx.handleDictDataInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.dictDataInfoFormRef} */ ;
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
    field: "label",
    label: "数据标签",
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        field: "label",
        label: "数据标签",
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    modelValue: (__VLS_ctx.dictDataInfoForm.label),
    placeholder: "请输入数据标签",
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dictDataInfoForm.label),
        placeholder: "请输入数据标签",
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
    field: "value",
    label: "数据值",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        field: "value",
        label: "数据值",
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
__VLS_57.slots.default;
var __VLS_58 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
    modelValue: (__VLS_ctx.dictDataInfoForm.value),
    placeholder: "请输入数据值",
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dictDataInfoForm.value),
        placeholder: "请输入数据值",
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
    field: "displaySeq",
    label: "显示顺序",
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        field: "displaySeq",
        label: "显示顺序",
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
__VLS_69.slots.default;
var __VLS_70 = {}.AInputNumber;
/** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
    modelValue: (__VLS_ctx.dictDataInfoForm.displaySeq),
    placeholder: "请输入显示顺序",
    min: (1),
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dictDataInfoForm.displaySeq),
        placeholder: "请输入显示顺序",
        min: (1),
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
    field: "enable",
    label: "状态",
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        field: "enable",
        label: "状态",
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_82 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
    modelValue: (__VLS_ctx.dictDataInfoForm.enable),
}));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dictDataInfoForm.enable),
    }], __VLS_functionalComponentArgsRest(__VLS_83), false));
__VLS_85.slots.default;
var __VLS_86 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
    value: (true),
}));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
        value: (true),
    }], __VLS_functionalComponentArgsRest(__VLS_87), false));
__VLS_89.slots.default;
var __VLS_89;
var __VLS_90 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
    value: (false),
}));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
        value: (false),
    }], __VLS_functionalComponentArgsRest(__VLS_91), false));
__VLS_93.slots.default;
var __VLS_93;
var __VLS_85;
var __VLS_81;
var __VLS_77;
var __VLS_37;
var __VLS_94 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
    hideLabel: true,
}));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_95), false));
__VLS_97.slots.default;
var __VLS_98 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({}));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_99), false));
__VLS_101.slots.default;
var __VLS_102 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_103), false));
__VLS_105.slots.default;
var __VLS_105;
var __VLS_106 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106(__assign({ 'onClick': {} })));
var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_107), false));
var __VLS_110;
var __VLS_111;
var __VLS_112;
var __VLS_113 = {
    onClick: (__VLS_ctx.handleResetDictDataInfoForm)
};
__VLS_109.slots.default;
var __VLS_109;
var __VLS_101;
var __VLS_97;
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
