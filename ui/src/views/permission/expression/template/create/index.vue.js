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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "create-tile" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_8 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    model: (__VLS_ctx.templateInfoForm),
    rules: (__VLS_ctx.templateInfoFormRules),
    ref: "templateInfoFormRef",
    layout: "vertical",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.templateInfoForm),
        rules: (__VLS_ctx.templateInfoFormRules),
        ref: "templateInfoFormRef",
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
/** @type {typeof __VLS_ctx.templateInfoFormRef} */ ;
var __VLS_12 = {};
__VLS_11.slots.default;
var __VLS_14 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_15 = __VLS_asFunctionalComponent(__VLS_14, new __VLS_14({
    field: "name",
    label: "模板名称",
}));
var __VLS_16 = __VLS_15.apply(void 0, __spreadArray([{
        field: "name",
        label: "模板名称",
    }], __VLS_functionalComponentArgsRest(__VLS_15), false));
__VLS_17.slots.default;
var __VLS_18 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18({
    modelValue: (__VLS_ctx.templateInfoForm.name),
    placeholder: "请输入模板名称",
}));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.templateInfoForm.name),
        placeholder: "请输入模板名称",
    }], __VLS_functionalComponentArgsRest(__VLS_19), false));
var __VLS_17;
var __VLS_22 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_23 = __VLS_asFunctionalComponent(__VLS_22, new __VLS_22({
    field: "desc",
    label: "模板描述",
}));
var __VLS_24 = __VLS_23.apply(void 0, __spreadArray([{
        field: "desc",
        label: "模板描述",
    }], __VLS_functionalComponentArgsRest(__VLS_23), false));
__VLS_25.slots.default;
var __VLS_26 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26({
    modelValue: (__VLS_ctx.templateInfoForm.desc),
    placeholder: "请输入模板描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.templateInfoForm.desc),
        placeholder: "请输入模板描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_27), false));
var __VLS_25;
var __VLS_30 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30({
    field: "expression",
    label: "JEXL 表达式",
}));
var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([{
        field: "expression",
        label: "JEXL 表达式",
    }], __VLS_functionalComponentArgsRest(__VLS_31), false));
__VLS_33.slots.default;
var __VLS_34 = {}.MonacoEditor;
/** @type {[typeof __VLS_components.MonacoEditor, typeof __VLS_components.monacoEditor, ]} */ ;
// @ts-ignore
var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
    modelValue: (__VLS_ctx.templateInfoForm.expression),
    language: "jexl",
    height: "280px",
    editorOption: ({
        contextmenu: false,
        theme: 'jexl-vs-theme',
    }),
}));
var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.templateInfoForm.expression),
        language: "jexl",
        height: "280px",
        editorOption: ({
            contextmenu: false,
            theme: 'jexl-vs-theme',
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_35), false));
var __VLS_33;
var _loop_1 = function (item, index) {
    var __VLS_38 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
        field: ("paramConfigs[".concat(index, "]")),
        label: ("\u6A21\u677F\u53C2\u6570 - ".concat(index + 1)),
        key: (index),
    }));
    var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
            field: ("paramConfigs[".concat(index, "]")),
            label: ("\u6A21\u677F\u53C2\u6570 - ".concat(index + 1)),
            key: (index),
        }], __VLS_functionalComponentArgsRest(__VLS_39), false));
    __VLS_41.slots.default;
    var __VLS_42 = {}.ParamSelect;
    /** @type {[typeof __VLS_components.ParamSelect, ]} */ ;
    // @ts-ignore
    var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42(__assign({ 'onRemove': {} }, { ref: (function (el) {
            if (el)
                __VLS_ctx.paramConfigRefs[index] = el;
        }), modelValue: (__VLS_ctx.templateInfoForm.paramConfigs[index]) })));
    var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([__assign({ 'onRemove': {} }, { ref: (function (el) {
                if (el)
                    __VLS_ctx.paramConfigRefs[index] = el;
            }), modelValue: (__VLS_ctx.templateInfoForm.paramConfigs[index]) })], __VLS_functionalComponentArgsRest(__VLS_43), false));
    var __VLS_46 = void 0;
    var __VLS_47 = void 0;
    var __VLS_48 = void 0;
    var __VLS_49 = {
        onRemove: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleRemoveParamConfig(index);
        }
    };
};
var __VLS_45, __VLS_41;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.templateInfoForm.paramConfigs)); _i < _a.length; _i++) {
    var _b = _a[_i], item = _b[0], index = _b[1];
    _loop_1(item, index);
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "add-param-config" }));
var __VLS_50 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50(__assign({ 'onClick': {} }, { type: "text" })));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text" })], __VLS_functionalComponentArgsRest(__VLS_51), false));
var __VLS_54;
var __VLS_55;
var __VLS_56;
var __VLS_57 = {
    onClick: (__VLS_ctx.handleParamConfigModalOpen)
};
__VLS_53.slots.default;
{
    var __VLS_thisSlot = __VLS_53.slots.icon;
    var __VLS_58 = {}.IconPlus;
    /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
    // @ts-ignore
    var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({}));
    var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_59), false));
}
{
    var __VLS_thisSlot = __VLS_53.slots.default;
}
var __VLS_53;
var __VLS_62 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    hideLabel: true,
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
__VLS_65.slots.default;
var __VLS_66 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_67), false));
__VLS_69.slots.default;
var __VLS_70 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_71), false));
var __VLS_74;
var __VLS_75;
var __VLS_76;
var __VLS_77 = {
    onClick: (__VLS_ctx.handleTemplateInfoFormSubmit)
};
__VLS_73.slots.default;
var __VLS_73;
var __VLS_78 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78(__assign({ 'onClick': {} })));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_79), false));
var __VLS_82;
var __VLS_83;
var __VLS_84;
var __VLS_85 = {
    onClick: (__VLS_ctx.handleResetTemplateInfoForm)
};
__VLS_81.slots.default;
var __VLS_81;
var __VLS_69;
var __VLS_65;
var __VLS_11;
var __VLS_3;
var __VLS_86 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86(__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.addParamConfigModalVisible), maskClosable: (false), hideCancel: (true) })));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.addParamConfigModalVisible), maskClosable: (false), hideCancel: (true) })], __VLS_functionalComponentArgsRest(__VLS_87), false));
var __VLS_90;
var __VLS_91;
var __VLS_92;
var __VLS_93 = {
    onOk: (__VLS_ctx.handleParamConfigModalConfirm)
};
var __VLS_94 = {
    onCancel: (__VLS_ctx.handleParamConfigModalClose)
};
__VLS_89.slots.default;
{
    var __VLS_thisSlot = __VLS_89.slots.title;
}
var __VLS_95 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_96 = __VLS_asFunctionalComponent(__VLS_95, new __VLS_95({
    modelValue: (__VLS_ctx.selectedParamType),
    placeholder: "请选择参数类型",
}));
var __VLS_97 = __VLS_96.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.selectedParamType),
        placeholder: "请选择参数类型",
    }], __VLS_functionalComponentArgsRest(__VLS_96), false));
__VLS_98.slots.default;
for (var _c = 0, _d = __VLS_getVForSourceType((__VLS_ctx.parmaTypes)); _c < _d.length; _c++) {
    var _e = _d[_c], item = _e[0], index = _e[1];
    var __VLS_99 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_100 = __VLS_asFunctionalComponent(__VLS_99, new __VLS_99({
        value: (item.value),
    }));
    var __VLS_101 = __VLS_100.apply(void 0, __spreadArray([{
            value: (item.value),
        }], __VLS_functionalComponentArgsRest(__VLS_100), false));
    __VLS_102.slots.default;
    (item.label);
    var __VLS_102;
}
var __VLS_98;
var __VLS_89;
/** @type {__VLS_StyleScopedClasses['create-tile']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['add-param-config']} */ ;
// @ts-ignore
var __VLS_13 = __VLS_12;
var __VLS_dollars;
var __VLS_self;
