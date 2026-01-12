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
import createTs from "./index";
export default createTs;
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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "create-tile" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_8 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    model: (__VLS_ctx.createPermissionExpInfoForm),
    ref: "createPermissionExpInfoFormRef",
    rules: (__VLS_ctx.createPermissionExpInfoFormRules),
    layout: "vertical",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.createPermissionExpInfoForm),
        ref: "createPermissionExpInfoFormRef",
        rules: (__VLS_ctx.createPermissionExpInfoFormRules),
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
/** @type {typeof __VLS_ctx.createPermissionExpInfoFormRef} */ ;
var __VLS_12 = {};
__VLS_11.slots.default;
var __VLS_14 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_15 = __VLS_asFunctionalComponent(__VLS_14, new __VLS_14({
    field: "name",
    label: "限制条件名称",
}));
var __VLS_16 = __VLS_15.apply(void 0, __spreadArray([{
        field: "name",
        label: "限制条件名称",
    }], __VLS_functionalComponentArgsRest(__VLS_15), false));
__VLS_17.slots.default;
var __VLS_18 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18({
    modelValue: (__VLS_ctx.createPermissionExpInfoForm.name),
    placeholder: "请输入限制条件名称",
}));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createPermissionExpInfoForm.name),
        placeholder: "请输入限制条件名称",
    }], __VLS_functionalComponentArgsRest(__VLS_19), false));
var __VLS_17;
var __VLS_22 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_23 = __VLS_asFunctionalComponent(__VLS_22, new __VLS_22({
    field: "templateId",
    label: "限制条件模板",
}));
var __VLS_24 = __VLS_23.apply(void 0, __spreadArray([{
        field: "templateId",
        label: "限制条件模板",
    }], __VLS_functionalComponentArgsRest(__VLS_23), false));
__VLS_25.slots.default;
var __VLS_26 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26(__assign({ 'onChange': {} }, { placeholder: "请选择限制条件模板", allowSearch: true, allowClear: true, modelValue: (__VLS_ctx.createPermissionExpInfoForm.templateId) })));
var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { placeholder: "请选择限制条件模板", allowSearch: true, allowClear: true, modelValue: (__VLS_ctx.createPermissionExpInfoForm.templateId) })], __VLS_functionalComponentArgsRest(__VLS_27), false));
var __VLS_30;
var __VLS_31;
var __VLS_32;
var __VLS_33 = {
    onChange: (__VLS_ctx.handleTemplateSelectChange)
};
__VLS_29.slots.default;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.templateList)); _i < _a.length; _i++) {
    var item = _a[_i][0];
    var __VLS_34 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
        value: (item.id),
    }));
    var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
            value: (item.id),
        }], __VLS_functionalComponentArgsRest(__VLS_35), false));
    __VLS_37.slots.default;
    (item.name);
    var __VLS_37;
}
var __VLS_29;
var __VLS_25;
if (__VLS_ctx.templateParamConfigs.length > 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_38 = {}.ParamInput;
    /** @type {[typeof __VLS_components.ParamInput, ]} */ ;
    // @ts-ignore
    var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
        ref: "templateParamsRef",
        configs: (__VLS_ctx.templateParamConfigs),
        modelValue: (__VLS_ctx.createPermissionExpInfoForm.templateParams),
    }));
    var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
            ref: "templateParamsRef",
            configs: (__VLS_ctx.templateParamConfigs),
            modelValue: (__VLS_ctx.createPermissionExpInfoForm.templateParams),
        }], __VLS_functionalComponentArgsRest(__VLS_39), false));
    /** @type {typeof __VLS_ctx.templateParamsRef} */ ;
    var __VLS_42 = {};
    var __VLS_41;
}
if (!__VLS_ctx.createPermissionExpInfoForm.templateId) {
    var __VLS_44 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
        field: "expression",
        label: "JEXL 表达式",
    }));
    var __VLS_46 = __VLS_45.apply(void 0, __spreadArray([{
            field: "expression",
            label: "JEXL 表达式",
        }], __VLS_functionalComponentArgsRest(__VLS_45), false));
    __VLS_47.slots.default;
    var __VLS_48 = {}.MonacoEditor;
    /** @type {[typeof __VLS_components.MonacoEditor, typeof __VLS_components.monacoEditor, ]} */ ;
    // @ts-ignore
    var __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
        modelValue: (__VLS_ctx.createPermissionExpInfoForm.expression),
        language: "jexl",
        editorOption: ({
            contextmenu: false,
        }),
        height: "280px",
    }));
    var __VLS_50 = __VLS_49.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.createPermissionExpInfoForm.expression),
            language: "jexl",
            editorOption: ({
                contextmenu: false,
            }),
            height: "280px",
        }], __VLS_functionalComponentArgsRest(__VLS_49), false));
    var __VLS_47;
}
var __VLS_52 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    field: "desc",
    label: "限制条件描述",
}));
var __VLS_54 = __VLS_53.apply(void 0, __spreadArray([{
        field: "desc",
        label: "限制条件描述",
    }], __VLS_functionalComponentArgsRest(__VLS_53), false));
__VLS_55.slots.default;
var __VLS_56 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    modelValue: (__VLS_ctx.createPermissionExpInfoForm.desc),
    placeholder: "请输入限制条件描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_58 = __VLS_57.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createPermissionExpInfoForm.desc),
        placeholder: "请输入限制条件描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_57), false));
var __VLS_55;
var __VLS_60 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    hideLabel: true,
}));
var __VLS_62 = __VLS_61.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_61), false));
__VLS_63.slots.default;
var __VLS_64 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({}));
var __VLS_66 = __VLS_65.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_65), false));
__VLS_67.slots.default;
var __VLS_68 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_70 = __VLS_69.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_69), false));
var __VLS_72;
var __VLS_73;
var __VLS_74;
var __VLS_75 = {
    onClick: (__VLS_ctx.handleCreatePermissionExpInfoFormSubmit)
};
__VLS_71.slots.default;
var __VLS_71;
var __VLS_76 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76(__assign({ 'onClick': {} })));
var __VLS_78 = __VLS_77.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_77), false));
var __VLS_80;
var __VLS_81;
var __VLS_82;
var __VLS_83 = {
    onClick: (__VLS_ctx.handleResetCreatePermissionExpInfoForm)
};
__VLS_79.slots.default;
var __VLS_79;
var __VLS_67;
var __VLS_63;
var __VLS_11;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['create-tile']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
// @ts-ignore
var __VLS_13 = __VLS_12, __VLS_43 = __VLS_42;
var __VLS_dollars;
var __VLS_self;
