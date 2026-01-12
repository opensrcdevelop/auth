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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "setting-header" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "left" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
var __VLS_0 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onChange: (__VLS_ctx.handleTabChange)
};
__VLS_3.slots.default;
var __VLS_8 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    key: "jwt_secret",
    title: "JWT 密钥",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        key: "jwt_secret",
        title: "JWT 密钥",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
__VLS_11.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_12 = {}.ADescriptions;
/** @type {[typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    bordered: true,
    column: (2),
}));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{
        bordered: true,
        column: (2),
    }], __VLS_functionalComponentArgsRest(__VLS_13), false));
__VLS_15.slots.default;
var __VLS_16 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    label: "ID",
}));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([{
        label: "ID",
    }], __VLS_functionalComponentArgsRest(__VLS_17), false));
__VLS_19.slots.default;
(__VLS_ctx.secretInfo.kid);
var __VLS_19;
var __VLS_20 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
    label: "签名算法",
}));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{
        label: "签名算法",
    }], __VLS_functionalComponentArgsRest(__VLS_21), false));
__VLS_23.slots.default;
(__VLS_ctx.secretInfo.alg);
var __VLS_23;
var __VLS_24 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
    label: "创建时间",
}));
var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([{
        label: "创建时间",
    }], __VLS_functionalComponentArgsRest(__VLS_25), false));
__VLS_27.slots.default;
(__VLS_ctx.secretInfo.createTime);
var __VLS_27;
var __VLS_28 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    label: "过期时间",
}));
var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
        label: "过期时间",
    }], __VLS_functionalComponentArgsRest(__VLS_29), false));
__VLS_31.slots.default;
(__VLS_ctx.secretInfo.expireTime);
var __VLS_31;
var __VLS_15;
var __VLS_32 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32(__assign(__assign({ 'onClick': {} }, { type: "primary" }), { style: {} })));
var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { type: "primary" }), { style: {} })], __VLS_functionalComponentArgsRest(__VLS_33), false));
var __VLS_36;
var __VLS_37;
var __VLS_38;
var __VLS_39 = {
    onClick: (__VLS_ctx.handleRotateSecret)
};
__VLS_35.slots.default;
var __VLS_35;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_40 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.rotationConfigForm), ref: "rotationConfigFormRef", rules: (__VLS_ctx.rotationConfigFormRules), layout: "vertical" })));
var __VLS_42 = __VLS_41.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.rotationConfigForm), ref: "rotationConfigFormRef", rules: (__VLS_ctx.rotationConfigFormRules), layout: "vertical" })], __VLS_functionalComponentArgsRest(__VLS_41), false));
var __VLS_44;
var __VLS_45;
var __VLS_46;
var __VLS_47 = {
    onSubmitSuccess: (__VLS_ctx.handleRotationConfigFormSubmit)
};
/** @type {typeof __VLS_ctx.rotationConfigFormRef} */ ;
var __VLS_48 = {};
__VLS_43.slots.default;
var __VLS_50 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
    gutter: (24),
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
__VLS_53.slots.default;
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
    label: "轮换周期",
    field: "rotationPeriod",
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        label: "轮换周期",
        field: "rotationPeriod",
    }], __VLS_functionalComponentArgsRest(__VLS_59), false));
__VLS_61.slots.default;
var __VLS_62 = {}.AInputNumber;
/** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    modelValue: (__VLS_ctx.rotationConfigForm.rotationPeriod),
    min: (1),
    max: (99),
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.rotationConfigForm.rotationPeriod),
        min: (1),
        max: (99),
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
var __VLS_61;
var __VLS_57;
var __VLS_66 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
    span: (12),
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
__VLS_69.slots.default;
var __VLS_70 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
    label: "轮换周期单位",
    field: "rotationPeriodUnit",
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        label: "轮换周期单位",
        field: "rotationPeriodUnit",
    }], __VLS_functionalComponentArgsRest(__VLS_71), false));
__VLS_73.slots.default;
var __VLS_74 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    modelValue: (__VLS_ctx.rotationConfigForm.rotationPeriodUnit),
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.rotationConfigForm.rotationPeriodUnit),
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
__VLS_77.slots.default;
var __VLS_78 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
    value: "DAY",
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        value: "DAY",
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_81;
var __VLS_82 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
    value: "MONTH",
}));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
        value: "MONTH",
    }], __VLS_functionalComponentArgsRest(__VLS_83), false));
__VLS_85.slots.default;
var __VLS_85;
var __VLS_86 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
    value: "YEAR",
}));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
        value: "YEAR",
    }], __VLS_functionalComponentArgsRest(__VLS_87), false));
__VLS_89.slots.default;
var __VLS_89;
var __VLS_77;
var __VLS_73;
var __VLS_69;
var __VLS_53;
var __VLS_90 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
    hideLabel: true,
}));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_91), false));
__VLS_93.slots.default;
var __VLS_94 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({}));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_95), false));
__VLS_97.slots.default;
var __VLS_98 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_99), false));
__VLS_101.slots.default;
var __VLS_101;
var __VLS_102 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102(__assign({ 'onClick': {} })));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_103), false));
var __VLS_106;
var __VLS_107;
var __VLS_108;
var __VLS_109 = {
    onClick: (__VLS_ctx.handleResetRotationConfigForm)
};
__VLS_105.slots.default;
var __VLS_105;
var __VLS_97;
var __VLS_93;
var __VLS_43;
var __VLS_11;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['setting-header']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
// @ts-ignore
var __VLS_49 = __VLS_48;
var __VLS_dollars;
var __VLS_self;
