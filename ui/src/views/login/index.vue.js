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
import loginTs from "./index";
export default loginTs;
debugger; /* PartiallyEnd: #3632/script.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "login-container" }));
if (!__VLS_ctx.toMfa && !__VLS_ctx.toFogotPwd) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "form-container" }));
    if (__VLS_ctx.tenantName) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tenant-name" }));
        var __VLS_0 = {}.IconFont;
        /** @type {[typeof __VLS_components.IconFont, typeof __VLS_components.iconFont, ]} */ ;
        // @ts-ignore
        var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ type: "icon-tenant" }, { style: {} })));
        var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ type: "icon-tenant" }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_1), false));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (__VLS_ctx.tenantName);
    }
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
    var __VLS_4 = {}.ATabs;
    /** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
    // @ts-ignore
    var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({}));
    var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_5), false));
    __VLS_7.slots.default;
    var __VLS_8 = {}.ATabPane;
    /** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
    // @ts-ignore
    var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
        key: "1",
        title: "密码登录",
    }));
    var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
            key: "1",
            title: "密码登录",
        }], __VLS_functionalComponentArgsRest(__VLS_9), false));
    __VLS_11.slots.default;
    var __VLS_12 = {}.AForm;
    /** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
    // @ts-ignore
    var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign({ 'onSubmitSuccess': {} }, { size: "large", model: (__VLS_ctx.passwordLoginForm), rules: (__VLS_ctx.passwordLoginRules) })));
    var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { size: "large", model: (__VLS_ctx.passwordLoginForm), rules: (__VLS_ctx.passwordLoginRules) })], __VLS_functionalComponentArgsRest(__VLS_13), false));
    var __VLS_16 = void 0;
    var __VLS_17 = void 0;
    var __VLS_18 = void 0;
    var __VLS_19 = {
        onSubmitSuccess: (__VLS_ctx.openCaptchaVerify)
    };
    __VLS_15.slots.default;
    var __VLS_20 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
        field: "username",
        hideLabel: true,
    }));
    var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{
            field: "username",
            hideLabel: true,
        }], __VLS_functionalComponentArgsRest(__VLS_21), false));
    __VLS_23.slots.default;
    var __VLS_24 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
        modelValue: (__VLS_ctx.passwordLoginForm.username),
        placeholder: "请输入手机号 / 邮箱 / 用户名",
    }));
    var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.passwordLoginForm.username),
            placeholder: "请输入手机号 / 邮箱 / 用户名",
        }], __VLS_functionalComponentArgsRest(__VLS_25), false));
    __VLS_27.slots.default;
    {
        var __VLS_thisSlot = __VLS_27.slots.prefix;
        var __VLS_28 = {}.IconUser;
        /** @type {[typeof __VLS_components.IconUser, typeof __VLS_components.iconUser, ]} */ ;
        // @ts-ignore
        var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({}));
        var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_29), false));
    }
    var __VLS_27;
    var __VLS_23;
    var __VLS_32 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
        field: "password",
        hideLabel: true,
    }));
    var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([{
            field: "password",
            hideLabel: true,
        }], __VLS_functionalComponentArgsRest(__VLS_33), false));
    __VLS_35.slots.default;
    var __VLS_36 = {}.AInputPassword;
    /** @type {[typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, ]} */ ;
    // @ts-ignore
    var __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
        modelValue: (__VLS_ctx.passwordLoginForm.password),
        placeholder: "请输入登录密码",
    }));
    var __VLS_38 = __VLS_37.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.passwordLoginForm.password),
            placeholder: "请输入登录密码",
        }], __VLS_functionalComponentArgsRest(__VLS_37), false));
    __VLS_39.slots.default;
    {
        var __VLS_thisSlot = __VLS_39.slots.prefix;
        var __VLS_40 = {}.IconLock;
        /** @type {[typeof __VLS_components.IconLock, typeof __VLS_components.iconLock, ]} */ ;
        // @ts-ignore
        var __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({}));
        var __VLS_42 = __VLS_41.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_41), false));
    }
    var __VLS_39;
    var __VLS_35;
    var __VLS_44 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
        hideLabel: true,
    }));
    var __VLS_46 = __VLS_45.apply(void 0, __spreadArray([{
            hideLabel: true,
        }], __VLS_functionalComponentArgsRest(__VLS_45), false));
    __VLS_47.slots.default;
    var __VLS_48 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48(__assign(__assign({ htmlType: "submit", type: "primary" }, { class: "login-btn" }), { loading: (__VLS_ctx.loginLoading) })));
    var __VLS_50 = __VLS_49.apply(void 0, __spreadArray([__assign(__assign({ htmlType: "submit", type: "primary" }, { class: "login-btn" }), { loading: (__VLS_ctx.loginLoading) })], __VLS_functionalComponentArgsRest(__VLS_49), false));
    __VLS_51.slots.default;
    var __VLS_51;
    var __VLS_47;
    var __VLS_15;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
    var __VLS_52 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
        modelValue: (__VLS_ctx.rememberMe),
    }));
    var __VLS_54 = __VLS_53.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.rememberMe),
        }], __VLS_functionalComponentArgsRest(__VLS_53), false));
    __VLS_55.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ style: {} }));
    var __VLS_55;
    var __VLS_56 = {}.ALink;
    /** @type {[typeof __VLS_components.ALink, typeof __VLS_components.aLink, typeof __VLS_components.ALink, typeof __VLS_components.aLink, ]} */ ;
    // @ts-ignore
    var __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56(__assign({ 'onClick': {} })));
    var __VLS_58 = __VLS_57.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_57), false));
    var __VLS_60 = void 0;
    var __VLS_61 = void 0;
    var __VLS_62 = void 0;
    var __VLS_63 = {
        onClick: (__VLS_ctx.handleToForgotPwd)
    };
    __VLS_59.slots.default;
    var __VLS_59;
    var __VLS_11;
    var __VLS_64 = {}.ATabPane;
    /** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
    // @ts-ignore
    var __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
        key: "2",
        title: "邮箱登录",
    }));
    var __VLS_66 = __VLS_65.apply(void 0, __spreadArray([{
            key: "2",
            title: "邮箱登录",
        }], __VLS_functionalComponentArgsRest(__VLS_65), false));
    __VLS_67.slots.default;
    var __VLS_68 = {}.AForm;
    /** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
    // @ts-ignore
    var __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68(__assign({ 'onSubmitSuccess': {} }, { ref: "emailLoginFormRef", size: "large", model: (__VLS_ctx.emailLoginForm), rules: (__VLS_ctx.emailLoginFormRules) })));
    var __VLS_70 = __VLS_69.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { ref: "emailLoginFormRef", size: "large", model: (__VLS_ctx.emailLoginForm), rules: (__VLS_ctx.emailLoginFormRules) })], __VLS_functionalComponentArgsRest(__VLS_69), false));
    var __VLS_72 = void 0;
    var __VLS_73 = void 0;
    var __VLS_74 = void 0;
    var __VLS_75 = {
        onSubmitSuccess: (__VLS_ctx.handleEmailLoginFormSubmit)
    };
    /** @type {typeof __VLS_ctx.emailLoginFormRef} */ ;
    var __VLS_76 = {};
    __VLS_71.slots.default;
    var __VLS_78 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
        field: "email",
        hideLabel: true,
    }));
    var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
            field: "email",
            hideLabel: true,
        }], __VLS_functionalComponentArgsRest(__VLS_79), false));
    __VLS_81.slots.default;
    var __VLS_82 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
        modelValue: (__VLS_ctx.emailLoginForm.email),
        placeholder: "请输入邮箱",
    }));
    var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.emailLoginForm.email),
            placeholder: "请输入邮箱",
        }], __VLS_functionalComponentArgsRest(__VLS_83), false));
    __VLS_85.slots.default;
    {
        var __VLS_thisSlot = __VLS_85.slots.prefix;
        var __VLS_86 = {}.IconUser;
        /** @type {[typeof __VLS_components.IconUser, typeof __VLS_components.iconUser, ]} */ ;
        // @ts-ignore
        var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({}));
        var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_87), false));
    }
    var __VLS_85;
    var __VLS_81;
    var __VLS_90 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
        field: "code",
        hideLabel: true,
    }));
    var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
            field: "code",
            hideLabel: true,
        }], __VLS_functionalComponentArgsRest(__VLS_91), false));
    __VLS_93.slots.default;
    var __VLS_94 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94(__assign({ style: {} })));
    var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_95), false));
    __VLS_97.slots.default;
    var __VLS_98 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
        span: (16),
    }));
    var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
            span: (16),
        }], __VLS_functionalComponentArgsRest(__VLS_99), false));
    __VLS_101.slots.default;
    var __VLS_102 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
        modelValue: (__VLS_ctx.emailLoginForm.code),
        placeholder: "请输入验证码",
    }));
    var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.emailLoginForm.code),
            placeholder: "请输入验证码",
        }], __VLS_functionalComponentArgsRest(__VLS_103), false));
    __VLS_105.slots.default;
    {
        var __VLS_thisSlot = __VLS_105.slots.prefix;
        var __VLS_106 = {}.IconSafe;
        /** @type {[typeof __VLS_components.IconSafe, typeof __VLS_components.iconSafe, ]} */ ;
        // @ts-ignore
        var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({}));
        var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_107), false));
    }
    var __VLS_105;
    var __VLS_101;
    var __VLS_110 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110({
        span: (8),
    }));
    var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([{
            span: (8),
        }], __VLS_functionalComponentArgsRest(__VLS_111), false));
    __VLS_113.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "send-code-btn-container" }));
    var __VLS_114 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114(__assign(__assign({ 'onClick': {} }, { style: {} }), { type: "outline", disabled: (__VLS_ctx.sendEmailCodeDisable) })));
    var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { style: {} }), { type: "outline", disabled: (__VLS_ctx.sendEmailCodeDisable) })], __VLS_functionalComponentArgsRest(__VLS_115), false));
    var __VLS_118 = void 0;
    var __VLS_119 = void 0;
    var __VLS_120 = void 0;
    var __VLS_121 = {
        onClick: (__VLS_ctx.handleSendEmailCode)
    };
    __VLS_117.slots.default;
    (__VLS_ctx.sendEmailCodeBtnText);
    var __VLS_117;
    var __VLS_113;
    var __VLS_97;
    var __VLS_93;
    var __VLS_122 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
        hideLabel: true,
    }));
    var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
            hideLabel: true,
        }], __VLS_functionalComponentArgsRest(__VLS_123), false));
    __VLS_125.slots.default;
    var __VLS_126 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126(__assign({ htmlType: "submit", type: "primary" }, { class: "login-btn" })));
    var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([__assign({ htmlType: "submit", type: "primary" }, { class: "login-btn" })], __VLS_functionalComponentArgsRest(__VLS_127), false));
    __VLS_129.slots.default;
    var __VLS_129;
    var __VLS_125;
    var __VLS_71;
    var __VLS_130 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130(__assign({ modelValue: (__VLS_ctx.rememberMe) }, { style: {} })));
    var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([__assign({ modelValue: (__VLS_ctx.rememberMe) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_131), false));
    __VLS_133.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ style: {} }));
    var __VLS_133;
    var __VLS_67;
    var __VLS_7;
    var __VLS_134 = {}.FederationLogin;
    /** @type {[typeof __VLS_components.FederationLogin, ]} */ ;
    // @ts-ignore
    var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134({}));
    var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_135), false));
}
if (__VLS_ctx.toMfa && !__VLS_ctx.toBind && !__VLS_ctx.toFogotPwd) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "form-container" }));
    var __VLS_138 = {}.ASpin;
    /** @type {[typeof __VLS_components.ASpin, typeof __VLS_components.aSpin, typeof __VLS_components.ASpin, typeof __VLS_components.aSpin, ]} */ ;
    // @ts-ignore
    var __VLS_139 = __VLS_asFunctionalComponent(__VLS_138, new __VLS_138(__assign({ style: {} }, { loading: (__VLS_ctx.mfaValidLoading) })));
    var __VLS_140 = __VLS_139.apply(void 0, __spreadArray([__assign({ style: {} }, { loading: (__VLS_ctx.mfaValidLoading) })], __VLS_functionalComponentArgsRest(__VLS_139), false));
    __VLS_141.slots.default;
    var __VLS_142 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_143 = __VLS_asFunctionalComponent(__VLS_142, new __VLS_142(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
    var __VLS_144 = __VLS_143.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_143), false));
    var __VLS_146 = void 0;
    var __VLS_147 = void 0;
    var __VLS_148 = void 0;
    var __VLS_149 = {
        onClick: (__VLS_ctx.backToLogin)
    };
    __VLS_145.slots.default;
    {
        var __VLS_thisSlot = __VLS_145.slots.icon;
        var __VLS_150 = {}.IconLeft;
        /** @type {[typeof __VLS_components.IconLeft, typeof __VLS_components.iconLeft, ]} */ ;
        // @ts-ignore
        var __VLS_151 = __VLS_asFunctionalComponent(__VLS_150, new __VLS_150({}));
        var __VLS_152 = __VLS_151.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_151), false));
    }
    var __VLS_145;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "mfa-info" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "mfa-code" }));
    var __VLS_154 = {}.AVerificationCode;
    /** @type {[typeof __VLS_components.AVerificationCode, typeof __VLS_components.aVerificationCode, ]} */ ;
    // @ts-ignore
    var __VLS_155 = __VLS_asFunctionalComponent(__VLS_154, new __VLS_154(__assign(__assign({ 'onFinish': {} }, { size: "large", modelValue: (__VLS_ctx.totpValidForm.code) }), { style: {} })));
    var __VLS_156 = __VLS_155.apply(void 0, __spreadArray([__assign(__assign({ 'onFinish': {} }, { size: "large", modelValue: (__VLS_ctx.totpValidForm.code) }), { style: {} })], __VLS_functionalComponentArgsRest(__VLS_155), false));
    var __VLS_158 = void 0;
    var __VLS_159 = void 0;
    var __VLS_160 = void 0;
    var __VLS_161 = {
        onFinish: (__VLS_ctx.handleTotpValidSubmit)
    };
    var __VLS_157;
    var __VLS_141;
}
if (__VLS_ctx.toMfa && __VLS_ctx.toBind && !__VLS_ctx.toFogotPwd) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "form-container" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_162 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_163 = __VLS_asFunctionalComponent(__VLS_162, new __VLS_162(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
    var __VLS_164 = __VLS_163.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_163), false));
    var __VLS_166 = void 0;
    var __VLS_167 = void 0;
    var __VLS_168 = void 0;
    var __VLS_169 = {
        onClick: (__VLS_ctx.backToLogin)
    };
    __VLS_165.slots.default;
    {
        var __VLS_thisSlot = __VLS_165.slots.icon;
        var __VLS_170 = {}.IconLeft;
        /** @type {[typeof __VLS_components.IconLeft, typeof __VLS_components.iconLeft, ]} */ ;
        // @ts-ignore
        var __VLS_171 = __VLS_asFunctionalComponent(__VLS_170, new __VLS_170({}));
        var __VLS_172 = __VLS_171.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_171), false));
    }
    var __VLS_165;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "mfa-info" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "mfa-code" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    __VLS_asFunctionalElement(__VLS_intrinsicElements.img)({
        src: (__VLS_ctx.qrCodeData),
    });
    var __VLS_174 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_175 = __VLS_asFunctionalComponent(__VLS_174, new __VLS_174(__assign(__assign({ 'onClick': {} }, { type: "text" }), { style: {} })));
    var __VLS_176 = __VLS_175.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { type: "text" }), { style: {} })], __VLS_functionalComponentArgsRest(__VLS_175), false));
    var __VLS_178 = void 0;
    var __VLS_179 = void 0;
    var __VLS_180 = void 0;
    var __VLS_181 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            if (!(__VLS_ctx.toMfa && __VLS_ctx.toBind && !__VLS_ctx.toFogotPwd))
                return;
            __VLS_ctx.toBind = false;
        }
    };
    __VLS_177.slots.default;
    var __VLS_177;
    var __VLS_182 = {}.ADivider;
    /** @type {[typeof __VLS_components.ADivider, typeof __VLS_components.aDivider, typeof __VLS_components.ADivider, typeof __VLS_components.aDivider, ]} */ ;
    // @ts-ignore
    var __VLS_183 = __VLS_asFunctionalComponent(__VLS_182, new __VLS_182({
        orientation: "center",
    }));
    var __VLS_184 = __VLS_183.apply(void 0, __spreadArray([{
            orientation: "center",
        }], __VLS_functionalComponentArgsRest(__VLS_183), false));
    __VLS_185.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "mfa-info" }));
    var __VLS_185;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "app-download-container" }));
    var __VLS_186 = {}.APopover;
    /** @type {[typeof __VLS_components.APopover, typeof __VLS_components.aPopover, typeof __VLS_components.APopover, typeof __VLS_components.aPopover, ]} */ ;
    // @ts-ignore
    var __VLS_187 = __VLS_asFunctionalComponent(__VLS_186, new __VLS_186({
        position: "lb",
    }));
    var __VLS_188 = __VLS_187.apply(void 0, __spreadArray([{
            position: "lb",
        }], __VLS_functionalComponentArgsRest(__VLS_187), false));
    __VLS_189.slots.default;
    var __VLS_190 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_191 = __VLS_asFunctionalComponent(__VLS_190, new __VLS_190({
        type: "text",
        size: "mini",
    }));
    var __VLS_192 = __VLS_191.apply(void 0, __spreadArray([{
            type: "text",
            size: "mini",
        }], __VLS_functionalComponentArgsRest(__VLS_191), false));
    __VLS_193.slots.default;
    var __VLS_193;
    {
        var __VLS_thisSlot = __VLS_189.slots.content;
        var __VLS_194 = {}.AQrcode;
        /** @type {[typeof __VLS_components.AQrcode, typeof __VLS_components.aQrcode, typeof __VLS_components.AQrcode, typeof __VLS_components.aQrcode, ]} */ ;
        // @ts-ignore
        var __VLS_195 = __VLS_asFunctionalComponent(__VLS_194, new __VLS_194({
            value: "https://apps.apple.com/cn/app/id983156458",
            bordered: (false),
        }));
        var __VLS_196 = __VLS_195.apply(void 0, __spreadArray([{
                value: "https://apps.apple.com/cn/app/id983156458",
                bordered: (false),
            }], __VLS_functionalComponentArgsRest(__VLS_195), false));
    }
    var __VLS_189;
    var __VLS_198 = {}.APopover;
    /** @type {[typeof __VLS_components.APopover, typeof __VLS_components.aPopover, typeof __VLS_components.APopover, typeof __VLS_components.aPopover, ]} */ ;
    // @ts-ignore
    var __VLS_199 = __VLS_asFunctionalComponent(__VLS_198, new __VLS_198({
        position: "lb",
    }));
    var __VLS_200 = __VLS_199.apply(void 0, __spreadArray([{
            position: "lb",
        }], __VLS_functionalComponentArgsRest(__VLS_199), false));
    __VLS_201.slots.default;
    var __VLS_202 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_203 = __VLS_asFunctionalComponent(__VLS_202, new __VLS_202({
        type: "text",
        size: "mini",
    }));
    var __VLS_204 = __VLS_203.apply(void 0, __spreadArray([{
            type: "text",
            size: "mini",
        }], __VLS_functionalComponentArgsRest(__VLS_203), false));
    __VLS_205.slots.default;
    var __VLS_205;
    {
        var __VLS_thisSlot = __VLS_201.slots.content;
        var __VLS_206 = {}.AQrcode;
        /** @type {[typeof __VLS_components.AQrcode, typeof __VLS_components.aQrcode, typeof __VLS_components.AQrcode, typeof __VLS_components.aQrcode, ]} */ ;
        // @ts-ignore
        var __VLS_207 = __VLS_asFunctionalComponent(__VLS_206, new __VLS_206({
            value: "https://www.lenovomm.com/appdetail/com.azure.authenticator/20197724",
            bordered: (false),
        }));
        var __VLS_208 = __VLS_207.apply(void 0, __spreadArray([{
                value: "https://www.lenovomm.com/appdetail/com.azure.authenticator/20197724",
                bordered: (false),
            }], __VLS_functionalComponentArgsRest(__VLS_207), false));
    }
    var __VLS_201;
}
if (__VLS_ctx.toFogotPwd) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "form-container" }));
    if (__VLS_ctx.toCheckForgotPwdCode) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "forgot-pwd-container" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "forgot-pwd-title" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "forgot-pwd-info" }));
        var __VLS_210 = {}.AForm;
        /** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
        // @ts-ignore
        var __VLS_211 = __VLS_asFunctionalComponent(__VLS_210, new __VLS_210(__assign({ 'onSubmitSuccess': {} }, { ref: "checkForgotPwdCodeFormRef", size: "large", model: (__VLS_ctx.checkForgotPwdCodeForm), rules: (__VLS_ctx.checkForgotPwdCodeFormRules) })));
        var __VLS_212 = __VLS_211.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { ref: "checkForgotPwdCodeFormRef", size: "large", model: (__VLS_ctx.checkForgotPwdCodeForm), rules: (__VLS_ctx.checkForgotPwdCodeFormRules) })], __VLS_functionalComponentArgsRest(__VLS_211), false));
        var __VLS_214 = void 0;
        var __VLS_215 = void 0;
        var __VLS_216 = void 0;
        var __VLS_217 = {
            onSubmitSuccess: (__VLS_ctx.handleCheckForgotPwdCodeFormSubmit)
        };
        /** @type {typeof __VLS_ctx.checkForgotPwdCodeFormRef} */ ;
        var __VLS_218 = {};
        __VLS_213.slots.default;
        var __VLS_220 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_221 = __VLS_asFunctionalComponent(__VLS_220, new __VLS_220({
            field: "username",
            hideLabel: true,
        }));
        var __VLS_222 = __VLS_221.apply(void 0, __spreadArray([{
                field: "username",
                hideLabel: true,
            }], __VLS_functionalComponentArgsRest(__VLS_221), false));
        __VLS_223.slots.default;
        var __VLS_224 = {}.AInput;
        /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
        // @ts-ignore
        var __VLS_225 = __VLS_asFunctionalComponent(__VLS_224, new __VLS_224({
            modelValue: (__VLS_ctx.checkForgotPwdCodeForm.username),
            placeholder: "请输入邮箱",
        }));
        var __VLS_226 = __VLS_225.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.checkForgotPwdCodeForm.username),
                placeholder: "请输入邮箱",
            }], __VLS_functionalComponentArgsRest(__VLS_225), false));
        __VLS_227.slots.default;
        {
            var __VLS_thisSlot = __VLS_227.slots.prefix;
            var __VLS_228 = {}.IconUser;
            /** @type {[typeof __VLS_components.IconUser, typeof __VLS_components.iconUser, ]} */ ;
            // @ts-ignore
            var __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228({}));
            var __VLS_230 = __VLS_229.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_229), false));
        }
        var __VLS_227;
        var __VLS_223;
        var __VLS_232 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_233 = __VLS_asFunctionalComponent(__VLS_232, new __VLS_232({
            field: "code",
            hideLabel: true,
        }));
        var __VLS_234 = __VLS_233.apply(void 0, __spreadArray([{
                field: "code",
                hideLabel: true,
            }], __VLS_functionalComponentArgsRest(__VLS_233), false));
        __VLS_235.slots.default;
        var __VLS_236 = {}.ARow;
        /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
        // @ts-ignore
        var __VLS_237 = __VLS_asFunctionalComponent(__VLS_236, new __VLS_236(__assign({ style: {} })));
        var __VLS_238 = __VLS_237.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_237), false));
        __VLS_239.slots.default;
        var __VLS_240 = {}.ACol;
        /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
        // @ts-ignore
        var __VLS_241 = __VLS_asFunctionalComponent(__VLS_240, new __VLS_240({
            span: (16),
        }));
        var __VLS_242 = __VLS_241.apply(void 0, __spreadArray([{
                span: (16),
            }], __VLS_functionalComponentArgsRest(__VLS_241), false));
        __VLS_243.slots.default;
        var __VLS_244 = {}.AInput;
        /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
        // @ts-ignore
        var __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244({
            modelValue: (__VLS_ctx.checkForgotPwdCodeForm.code),
            placeholder: "请输入验证码",
        }));
        var __VLS_246 = __VLS_245.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.checkForgotPwdCodeForm.code),
                placeholder: "请输入验证码",
            }], __VLS_functionalComponentArgsRest(__VLS_245), false));
        __VLS_247.slots.default;
        {
            var __VLS_thisSlot = __VLS_247.slots.prefix;
            var __VLS_248 = {}.IconSafe;
            /** @type {[typeof __VLS_components.IconSafe, typeof __VLS_components.iconSafe, ]} */ ;
            // @ts-ignore
            var __VLS_249 = __VLS_asFunctionalComponent(__VLS_248, new __VLS_248({}));
            var __VLS_250 = __VLS_249.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_249), false));
        }
        var __VLS_247;
        var __VLS_243;
        var __VLS_252 = {}.ACol;
        /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
        // @ts-ignore
        var __VLS_253 = __VLS_asFunctionalComponent(__VLS_252, new __VLS_252({
            span: (8),
        }));
        var __VLS_254 = __VLS_253.apply(void 0, __spreadArray([{
                span: (8),
            }], __VLS_functionalComponentArgsRest(__VLS_253), false));
        __VLS_255.slots.default;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "send-code-btn-container" }));
        var __VLS_256 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_257 = __VLS_asFunctionalComponent(__VLS_256, new __VLS_256(__assign(__assign({ 'onClick': {} }, { style: {} }), { type: "outline", disabled: (__VLS_ctx.sendForgotPwdEmailCodeDisable) })));
        var __VLS_258 = __VLS_257.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { style: {} }), { type: "outline", disabled: (__VLS_ctx.sendForgotPwdEmailCodeDisable) })], __VLS_functionalComponentArgsRest(__VLS_257), false));
        var __VLS_260 = void 0;
        var __VLS_261 = void 0;
        var __VLS_262 = void 0;
        var __VLS_263 = {
            onClick: (__VLS_ctx.handleSendForgotPwdEmailCode)
        };
        __VLS_259.slots.default;
        (__VLS_ctx.sendForgotPwdEmailCodeBtnText);
        var __VLS_259;
        var __VLS_255;
        var __VLS_239;
        var __VLS_235;
        var __VLS_264 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_265 = __VLS_asFunctionalComponent(__VLS_264, new __VLS_264({
            hideLabel: true,
        }));
        var __VLS_266 = __VLS_265.apply(void 0, __spreadArray([{
                hideLabel: true,
            }], __VLS_functionalComponentArgsRest(__VLS_265), false));
        __VLS_267.slots.default;
        var __VLS_268 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_269 = __VLS_asFunctionalComponent(__VLS_268, new __VLS_268(__assign({ htmlType: "submit", type: "primary" }, { class: "login-btn" })));
        var __VLS_270 = __VLS_269.apply(void 0, __spreadArray([__assign({ htmlType: "submit", type: "primary" }, { class: "login-btn" })], __VLS_functionalComponentArgsRest(__VLS_269), false));
        __VLS_271.slots.default;
        var __VLS_271;
        var __VLS_267;
        var __VLS_213;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "backup" }));
        var __VLS_272 = {}.ALink;
        /** @type {[typeof __VLS_components.ALink, typeof __VLS_components.aLink, typeof __VLS_components.ALink, typeof __VLS_components.aLink, ]} */ ;
        // @ts-ignore
        var __VLS_273 = __VLS_asFunctionalComponent(__VLS_272, new __VLS_272(__assign({ 'onClick': {} }, { size: "mini" })));
        var __VLS_274 = __VLS_273.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_273), false));
        var __VLS_276 = void 0;
        var __VLS_277 = void 0;
        var __VLS_278 = void 0;
        var __VLS_279 = {
            onClick: (__VLS_ctx.handleBackupToLogin)
        };
        __VLS_275.slots.default;
        var __VLS_275;
    }
    if (__VLS_ctx.toResetPwd) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "forgot-pwd-container" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "forgot-pwd-title" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "forgot-pwd-info" }));
        ("\u4F60\u6B63\u5728\u91CD\u7F6E ".concat(__VLS_ctx.resetPwdForm.username, " \u7684\u5BC6\u7801\u3002"));
        var __VLS_280 = {}.AForm;
        /** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
        // @ts-ignore
        var __VLS_281 = __VLS_asFunctionalComponent(__VLS_280, new __VLS_280(__assign({ 'onSubmitSuccess': {} }, { ref: "resetPwdFormRef", size: "large", model: (__VLS_ctx.resetPwdForm), rules: (__VLS_ctx.resetPwdFormRules) })));
        var __VLS_282 = __VLS_281.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { ref: "resetPwdFormRef", size: "large", model: (__VLS_ctx.resetPwdForm), rules: (__VLS_ctx.resetPwdFormRules) })], __VLS_functionalComponentArgsRest(__VLS_281), false));
        var __VLS_284 = void 0;
        var __VLS_285 = void 0;
        var __VLS_286 = void 0;
        var __VLS_287 = {
            onSubmitSuccess: (__VLS_ctx.handleResetPwdFormSubmit)
        };
        /** @type {typeof __VLS_ctx.resetPwdFormRef} */ ;
        var __VLS_288 = {};
        __VLS_283.slots.default;
        var __VLS_290 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_291 = __VLS_asFunctionalComponent(__VLS_290, new __VLS_290({
            field: "newPwd",
            label: "新密码",
            hideLabel: true,
        }));
        var __VLS_292 = __VLS_291.apply(void 0, __spreadArray([{
                field: "newPwd",
                label: "新密码",
                hideLabel: true,
            }], __VLS_functionalComponentArgsRest(__VLS_291), false));
        __VLS_293.slots.default;
        var __VLS_294 = {}.PasswordChecker;
        /** @type {[typeof __VLS_components.PasswordChecker, typeof __VLS_components.passwordChecker, ]} */ ;
        // @ts-ignore
        var __VLS_295 = __VLS_asFunctionalComponent(__VLS_294, new __VLS_294(__assign({ 'onCheck': {} }, { type: "password", placeholder: "请输入新密码", loading: (__VLS_ctx.checkLoading), checkRes: (__VLS_ctx.checkRes) })));
        var __VLS_296 = __VLS_295.apply(void 0, __spreadArray([__assign({ 'onCheck': {} }, { type: "password", placeholder: "请输入新密码", loading: (__VLS_ctx.checkLoading), checkRes: (__VLS_ctx.checkRes) })], __VLS_functionalComponentArgsRest(__VLS_295), false));
        var __VLS_298 = void 0;
        var __VLS_299 = void 0;
        var __VLS_300 = void 0;
        var __VLS_301 = {
            onCheck: (__VLS_ctx.handleCheckPassword)
        };
        var __VLS_297;
        var __VLS_293;
        var __VLS_302 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_303 = __VLS_asFunctionalComponent(__VLS_302, new __VLS_302({
            field: "confirmPwd",
            label: "确认密码",
            hideLabel: true,
        }));
        var __VLS_304 = __VLS_303.apply(void 0, __spreadArray([{
                field: "confirmPwd",
                label: "确认密码",
                hideLabel: true,
            }], __VLS_functionalComponentArgsRest(__VLS_303), false));
        __VLS_305.slots.default;
        var __VLS_306 = {}.AInputPassword;
        /** @type {[typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, ]} */ ;
        // @ts-ignore
        var __VLS_307 = __VLS_asFunctionalComponent(__VLS_306, new __VLS_306({
            modelValue: (__VLS_ctx.resetPwdForm.confirmPwd),
            placeholder: "请确认密码",
        }));
        var __VLS_308 = __VLS_307.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.resetPwdForm.confirmPwd),
                placeholder: "请确认密码",
            }], __VLS_functionalComponentArgsRest(__VLS_307), false));
        var __VLS_305;
        var __VLS_310 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_311 = __VLS_asFunctionalComponent(__VLS_310, new __VLS_310({
            hideLabel: true,
        }));
        var __VLS_312 = __VLS_311.apply(void 0, __spreadArray([{
                hideLabel: true,
            }], __VLS_functionalComponentArgsRest(__VLS_311), false));
        __VLS_313.slots.default;
        var __VLS_314 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_315 = __VLS_asFunctionalComponent(__VLS_314, new __VLS_314(__assign({ htmlType: "submit", type: "primary" }, { class: "login-btn" })));
        var __VLS_316 = __VLS_315.apply(void 0, __spreadArray([__assign({ htmlType: "submit", type: "primary" }, { class: "login-btn" })], __VLS_functionalComponentArgsRest(__VLS_315), false));
        __VLS_317.slots.default;
        var __VLS_317;
        var __VLS_313;
        var __VLS_283;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "backup" }));
        var __VLS_318 = {}.ALink;
        /** @type {[typeof __VLS_components.ALink, typeof __VLS_components.aLink, typeof __VLS_components.ALink, typeof __VLS_components.aLink, ]} */ ;
        // @ts-ignore
        var __VLS_319 = __VLS_asFunctionalComponent(__VLS_318, new __VLS_318(__assign({ 'onClick': {} }, { size: "mini" })));
        var __VLS_320 = __VLS_319.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_319), false));
        var __VLS_322 = void 0;
        var __VLS_323 = void 0;
        var __VLS_324 = void 0;
        var __VLS_325 = {
            onClick: (__VLS_ctx.handleBackToForgotPwd)
        };
        __VLS_321.slots.default;
        var __VLS_321;
    }
}
var __VLS_326 = {}.verify;
/** @type {[typeof __VLS_components.Verify, typeof __VLS_components.verify, ]} */ ;
// @ts-ignore
var __VLS_327 = __VLS_asFunctionalComponent(__VLS_326, new __VLS_326(__assign({ 'onSuccess': {} }, { mode: "pop", captchaType: "blockPuzzle", imgSize: ({ width: '330px', height: '155px' }), ref: "captchaVerifyRef" })));
var __VLS_328 = __VLS_327.apply(void 0, __spreadArray([__assign({ 'onSuccess': {} }, { mode: "pop", captchaType: "blockPuzzle", imgSize: ({ width: '330px', height: '155px' }), ref: "captchaVerifyRef" })], __VLS_functionalComponentArgsRest(__VLS_327), false));
var __VLS_330;
var __VLS_331;
var __VLS_332;
var __VLS_333 = {
    onSuccess: (__VLS_ctx.handlePasswordLoginFromSubmit)
};
/** @type {typeof __VLS_ctx.captchaVerifyRef} */ ;
var __VLS_334 = {};
var __VLS_329;
/** @type {__VLS_StyleScopedClasses['login-container']} */ ;
/** @type {__VLS_StyleScopedClasses['form-container']} */ ;
/** @type {__VLS_StyleScopedClasses['tenant-name']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['login-btn']} */ ;
/** @type {__VLS_StyleScopedClasses['send-code-btn-container']} */ ;
/** @type {__VLS_StyleScopedClasses['login-btn']} */ ;
/** @type {__VLS_StyleScopedClasses['form-container']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['mfa-info']} */ ;
/** @type {__VLS_StyleScopedClasses['mfa-code']} */ ;
/** @type {__VLS_StyleScopedClasses['form-container']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['mfa-info']} */ ;
/** @type {__VLS_StyleScopedClasses['mfa-code']} */ ;
/** @type {__VLS_StyleScopedClasses['mfa-info']} */ ;
/** @type {__VLS_StyleScopedClasses['app-download-container']} */ ;
/** @type {__VLS_StyleScopedClasses['form-container']} */ ;
/** @type {__VLS_StyleScopedClasses['forgot-pwd-container']} */ ;
/** @type {__VLS_StyleScopedClasses['forgot-pwd-title']} */ ;
/** @type {__VLS_StyleScopedClasses['forgot-pwd-info']} */ ;
/** @type {__VLS_StyleScopedClasses['send-code-btn-container']} */ ;
/** @type {__VLS_StyleScopedClasses['login-btn']} */ ;
/** @type {__VLS_StyleScopedClasses['backup']} */ ;
/** @type {__VLS_StyleScopedClasses['forgot-pwd-container']} */ ;
/** @type {__VLS_StyleScopedClasses['forgot-pwd-title']} */ ;
/** @type {__VLS_StyleScopedClasses['forgot-pwd-info']} */ ;
/** @type {__VLS_StyleScopedClasses['login-btn']} */ ;
/** @type {__VLS_StyleScopedClasses['backup']} */ ;
// @ts-ignore
var __VLS_77 = __VLS_76, __VLS_219 = __VLS_218, __VLS_289 = __VLS_288, __VLS_335 = __VLS_334;
var __VLS_dollars;
var __VLS_self;
