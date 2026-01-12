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
(__VLS_ctx.registrationName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.registrationId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.registrationId),
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
    key: "registration_info",
    title: "身份源信息",
}));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{
        key: "registration_info",
        title: "身份源信息",
    }], __VLS_functionalComponentArgsRest(__VLS_21), false));
__VLS_23.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_24 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.registrationInfoForm), layout: "vertical", ref: "registrationInfoFormRef", rules: (__VLS_ctx.registrationInfoFormRules) })));
var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.registrationInfoForm), layout: "vertical", ref: "registrationInfoFormRef", rules: (__VLS_ctx.registrationInfoFormRules) })], __VLS_functionalComponentArgsRest(__VLS_25), false));
var __VLS_28;
var __VLS_29;
var __VLS_30;
var __VLS_31 = {
    onSubmitSuccess: (__VLS_ctx.handleRegistrationInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.registrationInfoFormRef} */ ;
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
    label: "身份源显示名称",
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        field: "name",
        label: "身份源显示名称",
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    modelValue: (__VLS_ctx.registrationInfoForm.name),
    placeholder: "请输入身份源显示名称",
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.registrationInfoForm.name),
        placeholder: "请输入身份源显示名称",
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
    field: "code",
    label: "身份源标识",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        field: "code",
        label: "身份源标识",
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
__VLS_57.slots.default;
var __VLS_58 = {}.AInputGroup;
/** @type {[typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58(__assign({ style: {} })));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_59), false));
__VLS_61.slots.default;
var __VLS_62 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    modelValue: (__VLS_ctx.registrationInfoForm.code),
    placeholder: "请输入身份源标识",
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.registrationInfoForm.code),
        placeholder: "请输入身份源标识",
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
var __VLS_66 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66(__assign({ 'onClick': {} })));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_67), false));
var __VLS_70;
var __VLS_71;
var __VLS_72;
var __VLS_73 = {
    onClick: (__VLS_ctx.generateRandomRegistraionCode)
};
__VLS_69.slots.default;
var __VLS_69;
var __VLS_61;
var __VLS_57;
var __VLS_53;
var __VLS_37;
var __VLS_74 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    gutter: (24),
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
__VLS_77.slots.default;
var __VLS_78 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
    span: (12),
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_82 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
    field: "clientId",
    label: "Client ID",
}));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
        field: "clientId",
        label: "Client ID",
    }], __VLS_functionalComponentArgsRest(__VLS_83), false));
__VLS_85.slots.default;
var __VLS_86 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
    modelValue: (__VLS_ctx.registrationInfoForm.clientId),
    placeholder: "请输入 Client ID",
}));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.registrationInfoForm.clientId),
        placeholder: "请输入 Client ID",
    }], __VLS_functionalComponentArgsRest(__VLS_87), false));
var __VLS_85;
var __VLS_81;
var __VLS_90 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
    span: (12),
}));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_91), false));
__VLS_93.slots.default;
var __VLS_94 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
    field: "clientSecret",
    label: "Client Secret",
}));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
        field: "clientSecret",
        label: "Client Secret",
    }], __VLS_functionalComponentArgsRest(__VLS_95), false));
__VLS_97.slots.default;
var __VLS_98 = {}.AInputPassword;
/** @type {[typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
    modelValue: (__VLS_ctx.registrationInfoForm.clientSecret),
    placeholder: "请输入 Client Secret",
}));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.registrationInfoForm.clientSecret),
        placeholder: "请输入 Client Secret",
    }], __VLS_functionalComponentArgsRest(__VLS_99), false));
var __VLS_97;
var __VLS_93;
var __VLS_77;
var __VLS_102 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
    gutter: (24),
}));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_103), false));
__VLS_105.slots.default;
var __VLS_106 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({
    span: (12),
}));
var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_107), false));
__VLS_109.slots.default;
var __VLS_110 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110({
    field: "clientAuthenticationMethod",
    label: "客户端认证方式",
}));
var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([{
        field: "clientAuthenticationMethod",
        label: "客户端认证方式",
    }], __VLS_functionalComponentArgsRest(__VLS_111), false));
__VLS_113.slots.default;
var __VLS_114 = {}.ARadioGroup;
/** @type {[typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, ]} */ ;
// @ts-ignore
var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
    modelValue: (__VLS_ctx.registrationInfoForm.clientAuthenticationMethod),
}));
var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.registrationInfoForm.clientAuthenticationMethod),
    }], __VLS_functionalComponentArgsRest(__VLS_115), false));
__VLS_117.slots.default;
var __VLS_118 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
    value: "client_secret_basic",
}));
var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
        value: "client_secret_basic",
    }], __VLS_functionalComponentArgsRest(__VLS_119), false));
__VLS_121.slots.default;
var __VLS_121;
var __VLS_122 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
    value: "client_secret_post",
}));
var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
        value: "client_secret_post",
    }], __VLS_functionalComponentArgsRest(__VLS_123), false));
__VLS_125.slots.default;
var __VLS_125;
var __VLS_117;
var __VLS_113;
var __VLS_109;
var __VLS_126 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126({
    span: (12),
}));
var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_127), false));
__VLS_129.slots.default;
var __VLS_130 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130({
    field: "authorizationGrantType",
    label: "授权类型",
}));
var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{
        field: "authorizationGrantType",
        label: "授权类型",
    }], __VLS_functionalComponentArgsRest(__VLS_131), false));
__VLS_133.slots.default;
var __VLS_134 = {}.ARadioGroup;
/** @type {[typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, ]} */ ;
// @ts-ignore
var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134({
    modelValue: (__VLS_ctx.registrationInfoForm.authorizationGrantType),
}));
var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.registrationInfoForm.authorizationGrantType),
    }], __VLS_functionalComponentArgsRest(__VLS_135), false));
__VLS_137.slots.default;
var __VLS_138 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_139 = __VLS_asFunctionalComponent(__VLS_138, new __VLS_138({
    value: "authorization_code",
}));
var __VLS_140 = __VLS_139.apply(void 0, __spreadArray([{
        value: "authorization_code",
    }], __VLS_functionalComponentArgsRest(__VLS_139), false));
__VLS_141.slots.default;
var __VLS_141;
var __VLS_137;
var __VLS_133;
var __VLS_129;
var __VLS_105;
var __VLS_142 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_143 = __VLS_asFunctionalComponent(__VLS_142, new __VLS_142({
    label: "回调地址",
}));
var __VLS_144 = __VLS_143.apply(void 0, __spreadArray([{
        label: "回调地址",
    }], __VLS_functionalComponentArgsRest(__VLS_143), false));
__VLS_145.slots.default;
var __VLS_146 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_147 = __VLS_asFunctionalComponent(__VLS_146, new __VLS_146({
    text: (__VLS_ctx.callBackUrl),
}));
var __VLS_148 = __VLS_147.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.callBackUrl),
    }], __VLS_functionalComponentArgsRest(__VLS_147), false));
{
    var __VLS_thisSlot = __VLS_145.slots.extra;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
}
var __VLS_145;
var __VLS_150 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_151 = __VLS_asFunctionalComponent(__VLS_150, new __VLS_150({
    label: "额外参数",
    field: "additionalParams",
}));
var __VLS_152 = __VLS_151.apply(void 0, __spreadArray([{
        label: "额外参数",
        field: "additionalParams",
    }], __VLS_functionalComponentArgsRest(__VLS_151), false));
__VLS_153.slots.default;
var __VLS_154 = {}.MonacoEditor;
/** @type {[typeof __VLS_components.MonacoEditor, typeof __VLS_components.monacoEditor, ]} */ ;
// @ts-ignore
var __VLS_155 = __VLS_asFunctionalComponent(__VLS_154, new __VLS_154({
    modelValue: (__VLS_ctx.registrationInfoForm.additionalParams),
    language: "json",
    editorOption: ({
        contextmenu: false,
    }),
    height: "220px",
}));
var __VLS_156 = __VLS_155.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.registrationInfoForm.additionalParams),
        language: "json",
        editorOption: ({
            contextmenu: false,
        }),
        height: "220px",
    }], __VLS_functionalComponentArgsRest(__VLS_155), false));
var __VLS_153;
var __VLS_158 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_159 = __VLS_asFunctionalComponent(__VLS_158, new __VLS_158({
    hideLabel: true,
}));
var __VLS_160 = __VLS_159.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_159), false));
__VLS_161.slots.default;
var __VLS_162 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_163 = __VLS_asFunctionalComponent(__VLS_162, new __VLS_162({}));
var __VLS_164 = __VLS_163.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_163), false));
__VLS_165.slots.default;
var __VLS_166 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_167 = __VLS_asFunctionalComponent(__VLS_166, new __VLS_166({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_168 = __VLS_167.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_167), false));
__VLS_169.slots.default;
var __VLS_169;
var __VLS_170 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_171 = __VLS_asFunctionalComponent(__VLS_170, new __VLS_170(__assign({ 'onClick': {} })));
var __VLS_172 = __VLS_171.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_171), false));
var __VLS_174;
var __VLS_175;
var __VLS_176;
var __VLS_177 = {
    onClick: (__VLS_ctx.handleResetRegistrationInfoForm)
};
__VLS_173.slots.default;
var __VLS_173;
var __VLS_165;
var __VLS_161;
var __VLS_27;
var __VLS_23;
var __VLS_178 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_179 = __VLS_asFunctionalComponent(__VLS_178, new __VLS_178({
    key: "user_bindings",
    title: "关联用户",
}));
var __VLS_180 = __VLS_179.apply(void 0, __spreadArray([{
        key: "user_bindings",
        title: "关联用户",
    }], __VLS_functionalComponentArgsRest(__VLS_179), false));
__VLS_181.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_182 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_183 = __VLS_asFunctionalComponent(__VLS_182, new __VLS_182(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onKeyup': {} }), { style: ({ width: '320px', marginBottom: '16px' }) }), { placeholder: "输入用户名进行搜索", allowClear: true, modelValue: (__VLS_ctx.userBindingSearchKeyword) })));
var __VLS_184 = __VLS_183.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onKeyup': {} }), { style: ({ width: '320px', marginBottom: '16px' }) }), { placeholder: "输入用户名进行搜索", allowClear: true, modelValue: (__VLS_ctx.userBindingSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_183), false));
var __VLS_186;
var __VLS_187;
var __VLS_188;
var __VLS_189 = {
    onSearch: (__VLS_ctx.handleSearchUserBinding)
};
var __VLS_190 = {
    onClear: (__VLS_ctx.handleSearchUserBinding)
};
var __VLS_191 = {
    onKeyup: (__VLS_ctx.handleSearchUserBinding)
};
var __VLS_185;
var __VLS_192 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192(__assign(__assign({ 'onPageSizeChange': {} }, { 'onPageChange': {} }), { data: (__VLS_ctx.userBindingList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.userBindingListPagination.pagination) })));
var __VLS_194 = __VLS_193.apply(void 0, __spreadArray([__assign(__assign({ 'onPageSizeChange': {} }, { 'onPageChange': {} }), { data: (__VLS_ctx.userBindingList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.userBindingListPagination.pagination) })], __VLS_functionalComponentArgsRest(__VLS_193), false));
var __VLS_196;
var __VLS_197;
var __VLS_198;
var __VLS_199 = {
    onPageSizeChange: (__VLS_ctx.userBindingListPagination.handlePageSizeChange)
};
var __VLS_200 = {
    onPageChange: (__VLS_ctx.userBindingListPagination.handlePageChange)
};
__VLS_195.slots.default;
{
    var __VLS_thisSlot = __VLS_195.slots.columns;
    var __VLS_201 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_202 = __VLS_asFunctionalComponent(__VLS_201, new __VLS_201({
        title: "用户名",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_203 = __VLS_202.apply(void 0, __spreadArray([{
            title: "用户名",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_202), false));
    __VLS_204.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_204.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToUserDetail(record_1.userId);
            } }, { class: "table-column-name" }));
        (record_1.username);
    }
    var __VLS_204;
    var __VLS_205 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_206 = __VLS_asFunctionalComponent(__VLS_205, new __VLS_205({
        title: "唯一标识",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_207 = __VLS_206.apply(void 0, __spreadArray([{
            title: "唯一标识",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_206), false));
    __VLS_208.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_208.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.uniqueId);
    }
    var __VLS_208;
    var __VLS_209 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_210 = __VLS_asFunctionalComponent(__VLS_209, new __VLS_209({
        title: "绑定时间",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_211 = __VLS_210.apply(void 0, __spreadArray([{
            title: "绑定时间",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_210), false));
    __VLS_212.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_212.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.bindingTime);
    }
    var __VLS_212;
}
var __VLS_195;
var __VLS_181;
var __VLS_15;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
// @ts-ignore
var __VLS_33 = __VLS_32;
var __VLS_dollars;
var __VLS_self;
