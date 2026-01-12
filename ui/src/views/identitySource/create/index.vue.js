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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: (__VLS_ctx.identitySourceProvider.name ? 'provider-container' : '') }));
var __VLS_8 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    gutter: (24),
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
__VLS_11.slots.default;
var __VLS_12 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    span: (12),
}));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_13), false));
__VLS_15.slots.default;
if (__VLS_ctx.identitySourceProvider.name) {
    var __VLS_16 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
        modelValue: (__VLS_ctx.identitySourceProvider.name),
        disabled: true,
    }));
    var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.identitySourceProvider.name),
            disabled: true,
        }], __VLS_functionalComponentArgsRest(__VLS_17), false));
}
else {
    var __VLS_20 = {}.AForm;
    /** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
    // @ts-ignore
    var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
        model: (__VLS_ctx.identitySourceProvider),
        layout: "vertical",
        ref: "identitySourceProviderRef",
    }));
    var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{
            model: (__VLS_ctx.identitySourceProvider),
            layout: "vertical",
            ref: "identitySourceProviderRef",
        }], __VLS_functionalComponentArgsRest(__VLS_21), false));
    /** @type {typeof __VLS_ctx.identitySourceProviderRef} */ ;
    var __VLS_24 = {};
    __VLS_23.slots.default;
    var __VLS_26 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26({
        field: "id",
        hideLabel: true,
        rules: ([{ required: true, message: '身份提供商未选择' }]),
    }));
    var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([{
            field: "id",
            hideLabel: true,
            rules: ([{ required: true, message: '身份提供商未选择' }]),
        }], __VLS_functionalComponentArgsRest(__VLS_27), false));
    __VLS_29.slots.default;
    var __VLS_30 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30({
        modelValue: (__VLS_ctx.identitySourceProvider.id),
        placeholder: "请选择身份提供商",
        allowSearch: true,
        allowClear: true,
    }));
    var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.identitySourceProvider.id),
            placeholder: "请选择身份提供商",
            allowSearch: true,
            allowClear: true,
        }], __VLS_functionalComponentArgsRest(__VLS_31), false));
    __VLS_33.slots.default;
    for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.providerList)); _i < _a.length; _i++) {
        var provider = _a[_i][0];
        var __VLS_34 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
            key: (provider.id),
            value: (provider.id),
        }));
        var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
                key: (provider.id),
                value: (provider.id),
            }], __VLS_functionalComponentArgsRest(__VLS_35), false));
        __VLS_37.slots.default;
        (provider.name);
        var __VLS_37;
    }
    var __VLS_33;
    var __VLS_29;
    var __VLS_23;
}
var __VLS_15;
var __VLS_11;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_38 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
    model: (__VLS_ctx.createIdentitySourceForm),
    layout: "vertical",
    ref: "createIdentitySourceFormRef",
    rules: (__VLS_ctx.createIdentitySourceFormRules),
}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.createIdentitySourceForm),
        layout: "vertical",
        ref: "createIdentitySourceFormRef",
        rules: (__VLS_ctx.createIdentitySourceFormRules),
    }], __VLS_functionalComponentArgsRest(__VLS_39), false));
/** @type {typeof __VLS_ctx.createIdentitySourceFormRef} */ ;
var __VLS_42 = {};
__VLS_41.slots.default;
var __VLS_44 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    gutter: (24),
}));
var __VLS_46 = __VLS_45.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_45), false));
__VLS_47.slots.default;
var __VLS_48 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    span: (12),
}));
var __VLS_50 = __VLS_49.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_49), false));
__VLS_51.slots.default;
var __VLS_52 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({
    field: "name",
    label: "身份源显示名称",
}));
var __VLS_54 = __VLS_53.apply(void 0, __spreadArray([{
        field: "name",
        label: "身份源显示名称",
    }], __VLS_functionalComponentArgsRest(__VLS_53), false));
__VLS_55.slots.default;
var __VLS_56 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    modelValue: (__VLS_ctx.createIdentitySourceForm.name),
    placeholder: "请输入身份源显示名称",
}));
var __VLS_58 = __VLS_57.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createIdentitySourceForm.name),
        placeholder: "请输入身份源显示名称",
    }], __VLS_functionalComponentArgsRest(__VLS_57), false));
var __VLS_55;
var __VLS_51;
var __VLS_60 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    span: (12),
}));
var __VLS_62 = __VLS_61.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_61), false));
__VLS_63.slots.default;
var __VLS_64 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
    field: "code",
    label: "身份源标识",
}));
var __VLS_66 = __VLS_65.apply(void 0, __spreadArray([{
        field: "code",
        label: "身份源标识",
    }], __VLS_functionalComponentArgsRest(__VLS_65), false));
__VLS_67.slots.default;
var __VLS_68 = {}.AInputGroup;
/** @type {[typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, ]} */ ;
// @ts-ignore
var __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68(__assign({ style: {} })));
var __VLS_70 = __VLS_69.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_69), false));
__VLS_71.slots.default;
var __VLS_72 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
    modelValue: (__VLS_ctx.createIdentitySourceForm.code),
    placeholder: "请输入身份源标识",
}));
var __VLS_74 = __VLS_73.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createIdentitySourceForm.code),
        placeholder: "请输入身份源标识",
    }], __VLS_functionalComponentArgsRest(__VLS_73), false));
var __VLS_76 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76(__assign({ 'onClick': {} })));
var __VLS_78 = __VLS_77.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_77), false));
var __VLS_80;
var __VLS_81;
var __VLS_82;
var __VLS_83 = {
    onClick: (__VLS_ctx.generateRandomIdentitySourceCode)
};
__VLS_79.slots.default;
var __VLS_79;
var __VLS_71;
var __VLS_67;
var __VLS_63;
var __VLS_47;
var __VLS_84 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
    gutter: (24),
}));
var __VLS_86 = __VLS_85.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_85), false));
__VLS_87.slots.default;
var __VLS_88 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88({
    span: (12),
}));
var __VLS_90 = __VLS_89.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_89), false));
__VLS_91.slots.default;
var __VLS_92 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92({
    field: "clientId",
    label: "Client ID",
}));
var __VLS_94 = __VLS_93.apply(void 0, __spreadArray([{
        field: "clientId",
        label: "Client ID",
    }], __VLS_functionalComponentArgsRest(__VLS_93), false));
__VLS_95.slots.default;
var __VLS_96 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
    modelValue: (__VLS_ctx.createIdentitySourceForm.clientId),
    placeholder: "请输入 Client ID",
}));
var __VLS_98 = __VLS_97.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createIdentitySourceForm.clientId),
        placeholder: "请输入 Client ID",
    }], __VLS_functionalComponentArgsRest(__VLS_97), false));
var __VLS_95;
var __VLS_91;
var __VLS_100 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100({
    span: (12),
}));
var __VLS_102 = __VLS_101.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_101), false));
__VLS_103.slots.default;
var __VLS_104 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_105 = __VLS_asFunctionalComponent(__VLS_104, new __VLS_104({
    field: "clientSecret",
    label: "Client Secret",
}));
var __VLS_106 = __VLS_105.apply(void 0, __spreadArray([{
        field: "clientSecret",
        label: "Client Secret",
    }], __VLS_functionalComponentArgsRest(__VLS_105), false));
__VLS_107.slots.default;
var __VLS_108 = {}.AInputPassword;
/** @type {[typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, ]} */ ;
// @ts-ignore
var __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
    modelValue: (__VLS_ctx.createIdentitySourceForm.clientSecret),
    placeholder: "请输入 Client Secret",
}));
var __VLS_110 = __VLS_109.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createIdentitySourceForm.clientSecret),
        placeholder: "请输入 Client Secret",
    }], __VLS_functionalComponentArgsRest(__VLS_109), false));
var __VLS_107;
var __VLS_103;
var __VLS_87;
var __VLS_112 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({
    gutter: (24),
}));
var __VLS_114 = __VLS_113.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_113), false));
__VLS_115.slots.default;
var __VLS_116 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
    span: (12),
}));
var __VLS_118 = __VLS_117.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_117), false));
__VLS_119.slots.default;
var __VLS_120 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
    field: "clientAuthenticationMethod",
    label: "客户端认证方式",
}));
var __VLS_122 = __VLS_121.apply(void 0, __spreadArray([{
        field: "clientAuthenticationMethod",
        label: "客户端认证方式",
    }], __VLS_functionalComponentArgsRest(__VLS_121), false));
__VLS_123.slots.default;
var __VLS_124 = {}.ARadioGroup;
/** @type {[typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, ]} */ ;
// @ts-ignore
var __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({
    modelValue: (__VLS_ctx.createIdentitySourceForm.clientAuthenticationMethod),
}));
var __VLS_126 = __VLS_125.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createIdentitySourceForm.clientAuthenticationMethod),
    }], __VLS_functionalComponentArgsRest(__VLS_125), false));
__VLS_127.slots.default;
var __VLS_128 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_129 = __VLS_asFunctionalComponent(__VLS_128, new __VLS_128({
    value: "client_secret_basic",
}));
var __VLS_130 = __VLS_129.apply(void 0, __spreadArray([{
        value: "client_secret_basic",
    }], __VLS_functionalComponentArgsRest(__VLS_129), false));
__VLS_131.slots.default;
var __VLS_131;
var __VLS_132 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
    value: "client_secret_post",
}));
var __VLS_134 = __VLS_133.apply(void 0, __spreadArray([{
        value: "client_secret_post",
    }], __VLS_functionalComponentArgsRest(__VLS_133), false));
__VLS_135.slots.default;
var __VLS_135;
var __VLS_127;
var __VLS_123;
var __VLS_119;
var __VLS_136 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
    span: (12),
}));
var __VLS_138 = __VLS_137.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_137), false));
__VLS_139.slots.default;
var __VLS_140 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
    field: "authorizationGrantType",
    label: "授权类型",
}));
var __VLS_142 = __VLS_141.apply(void 0, __spreadArray([{
        field: "authorizationGrantType",
        label: "授权类型",
    }], __VLS_functionalComponentArgsRest(__VLS_141), false));
__VLS_143.slots.default;
var __VLS_144 = {}.ARadioGroup;
/** @type {[typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, ]} */ ;
// @ts-ignore
var __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
    modelValue: (__VLS_ctx.createIdentitySourceForm.authorizationGrantType),
}));
var __VLS_146 = __VLS_145.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createIdentitySourceForm.authorizationGrantType),
    }], __VLS_functionalComponentArgsRest(__VLS_145), false));
__VLS_147.slots.default;
var __VLS_148 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
    value: "authorization_code",
}));
var __VLS_150 = __VLS_149.apply(void 0, __spreadArray([{
        value: "authorization_code",
    }], __VLS_functionalComponentArgsRest(__VLS_149), false));
__VLS_151.slots.default;
var __VLS_151;
var __VLS_147;
var __VLS_143;
var __VLS_139;
var __VLS_115;
var __VLS_152 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152({
    label: "回调地址",
}));
var __VLS_154 = __VLS_153.apply(void 0, __spreadArray([{
        label: "回调地址",
    }], __VLS_functionalComponentArgsRest(__VLS_153), false));
__VLS_155.slots.default;
var __VLS_156 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156({
    text: (__VLS_ctx.callBackUrl),
}));
var __VLS_158 = __VLS_157.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.callBackUrl),
    }], __VLS_functionalComponentArgsRest(__VLS_157), false));
{
    var __VLS_thisSlot = __VLS_155.slots.extra;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
}
var __VLS_155;
var __VLS_160 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({
    label: "额外参数",
    field: "additionalParams",
}));
var __VLS_162 = __VLS_161.apply(void 0, __spreadArray([{
        label: "额外参数",
        field: "additionalParams",
    }], __VLS_functionalComponentArgsRest(__VLS_161), false));
__VLS_163.slots.default;
var __VLS_164 = {}.MonacoEditor;
/** @type {[typeof __VLS_components.MonacoEditor, typeof __VLS_components.monacoEditor, ]} */ ;
// @ts-ignore
var __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
    modelValue: (__VLS_ctx.createIdentitySourceForm.additionalParams),
    language: "json",
    editorOption: ({
        contextmenu: false,
    }),
    height: "220px",
}));
var __VLS_166 = __VLS_165.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createIdentitySourceForm.additionalParams),
        language: "json",
        editorOption: ({
            contextmenu: false,
        }),
        height: "220px",
    }], __VLS_functionalComponentArgsRest(__VLS_165), false));
var __VLS_163;
var __VLS_168 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
    hideLabel: true,
}));
var __VLS_170 = __VLS_169.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_169), false));
__VLS_171.slots.default;
var __VLS_172 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172({}));
var __VLS_174 = __VLS_173.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_173), false));
__VLS_175.slots.default;
var __VLS_176 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_178 = __VLS_177.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_177), false));
var __VLS_180;
var __VLS_181;
var __VLS_182;
var __VLS_183 = {
    onClick: (__VLS_ctx.handleCreateIdentitySourceFormSubmit)
};
__VLS_179.slots.default;
var __VLS_179;
var __VLS_184 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_185 = __VLS_asFunctionalComponent(__VLS_184, new __VLS_184(__assign({ 'onClick': {} })));
var __VLS_186 = __VLS_185.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_185), false));
var __VLS_188;
var __VLS_189;
var __VLS_190;
var __VLS_191 = {
    onClick: (__VLS_ctx.handleResetCreateIdentitySourceForm)
};
__VLS_187.slots.default;
var __VLS_187;
var __VLS_175;
var __VLS_171;
var __VLS_41;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['create-tile']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
// @ts-ignore
var __VLS_25 = __VLS_24, __VLS_43 = __VLS_42;
var __VLS_dollars;
var __VLS_self;
