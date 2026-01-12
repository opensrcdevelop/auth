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
import changePwdTs from "./index";
export default changePwdTs;
debugger; /* PartiallyEnd: #3632/script.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "change-pwd-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "form-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tip-info-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "change-pwd-title" }));
if (__VLS_ctx.type === '0') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "change-pwd-info" }));
}
if (__VLS_ctx.type === '1') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "change-pwd-info" }));
}
var __VLS_0 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.changePwdForm), layout: "vertical", rules: (__VLS_ctx.changePwdFormRules) })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.changePwdForm), layout: "vertical", rules: (__VLS_ctx.changePwdFormRules) })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onSubmitSuccess: (__VLS_ctx.handleChangePwdFormSubmit)
};
__VLS_3.slots.default;
var __VLS_8 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    field: "rawPwd",
    label: "原密码",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        field: "rawPwd",
        label: "原密码",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
__VLS_11.slots.default;
var __VLS_12 = {}.AInputPassword;
/** @type {[typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    modelValue: (__VLS_ctx.changePwdForm.rawPwd),
    placeholder: "请输入原密码",
}));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.changePwdForm.rawPwd),
        placeholder: "请输入原密码",
    }], __VLS_functionalComponentArgsRest(__VLS_13), false));
var __VLS_11;
var __VLS_16 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    field: "newPwd",
    label: "新密码",
}));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([{
        field: "newPwd",
        label: "新密码",
    }], __VLS_functionalComponentArgsRest(__VLS_17), false));
__VLS_19.slots.default;
var __VLS_20 = {}.PasswordChecker;
/** @type {[typeof __VLS_components.PasswordChecker, typeof __VLS_components.passwordChecker, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20(__assign({ 'onCheck': {} }, { type: "password", placeholder: "请输入新密码", loading: (__VLS_ctx.checkLoading), checkRes: (__VLS_ctx.checkRes) })));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign({ 'onCheck': {} }, { type: "password", placeholder: "请输入新密码", loading: (__VLS_ctx.checkLoading), checkRes: (__VLS_ctx.checkRes) })], __VLS_functionalComponentArgsRest(__VLS_21), false));
var __VLS_24;
var __VLS_25;
var __VLS_26;
var __VLS_27 = {
    onCheck: (__VLS_ctx.handleCheckPassword)
};
var __VLS_23;
var __VLS_19;
var __VLS_28 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    field: "confirmPwd",
    label: "确认密码",
}));
var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
        field: "confirmPwd",
        label: "确认密码",
    }], __VLS_functionalComponentArgsRest(__VLS_29), false));
__VLS_31.slots.default;
var __VLS_32 = {}.AInputPassword;
/** @type {[typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, ]} */ ;
// @ts-ignore
var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    modelValue: (__VLS_ctx.changePwdForm.confirmPwd),
    placeholder: "请确认密码",
}));
var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.changePwdForm.confirmPwd),
        placeholder: "请确认密码",
    }], __VLS_functionalComponentArgsRest(__VLS_33), false));
var __VLS_31;
var __VLS_36 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    hideLabel: true,
}));
var __VLS_38 = __VLS_37.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_37), false));
__VLS_39.slots.default;
var __VLS_40 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40(__assign({ htmlType: "submit", type: "primary" }, { class: "submit-btn" })));
var __VLS_42 = __VLS_41.apply(void 0, __spreadArray([__assign({ htmlType: "submit", type: "primary" }, { class: "submit-btn" })], __VLS_functionalComponentArgsRest(__VLS_41), false));
__VLS_43.slots.default;
var __VLS_43;
var __VLS_39;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['change-pwd-container']} */ ;
/** @type {__VLS_StyleScopedClasses['form-container']} */ ;
/** @type {__VLS_StyleScopedClasses['tip-info-container']} */ ;
/** @type {__VLS_StyleScopedClasses['change-pwd-title']} */ ;
/** @type {__VLS_StyleScopedClasses['change-pwd-info']} */ ;
/** @type {__VLS_StyleScopedClasses['change-pwd-info']} */ ;
/** @type {__VLS_StyleScopedClasses['submit-btn']} */ ;
var __VLS_dollars;
var __VLS_self;
