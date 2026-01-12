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
(__VLS_ctx.clientName);
var __VLS_8 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8(__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })], __VLS_functionalComponentArgsRest(__VLS_9), false));
var __VLS_12;
var __VLS_13;
var __VLS_14;
var __VLS_15 = {
    onChange: (__VLS_ctx.handleTabChange)
};
__VLS_11.slots.default;
var __VLS_16 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    key: "client_setting",
    title: "客户端配置",
}));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([{
        key: "client_setting",
        title: "客户端配置",
    }], __VLS_functionalComponentArgsRest(__VLS_17), false));
__VLS_19.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_20 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.clientBasicInfoForm), rules: (__VLS_ctx.clientBasicInfoFormRules), layout: "vertical", ref: "clientBasicInfoFormRef" })));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.clientBasicInfoForm), rules: (__VLS_ctx.clientBasicInfoFormRules), layout: "vertical", ref: "clientBasicInfoFormRef" })], __VLS_functionalComponentArgsRest(__VLS_21), false));
var __VLS_24;
var __VLS_25;
var __VLS_26;
var __VLS_27 = {
    onSubmitSuccess: (__VLS_ctx.handleClientBasicInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.clientBasicInfoFormRef} */ ;
var __VLS_28 = {};
__VLS_23.slots.default;
var __VLS_30 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30({
    field: "name",
    label: "客户端名称",
}));
var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([{
        field: "name",
        label: "客户端名称",
    }], __VLS_functionalComponentArgsRest(__VLS_31), false));
__VLS_33.slots.default;
var __VLS_34 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
    modelValue: (__VLS_ctx.clientBasicInfoForm.name),
    placeholder: "请输入客户端名称",
}));
var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.clientBasicInfoForm.name),
        placeholder: "请输入客户端名称",
    }], __VLS_functionalComponentArgsRest(__VLS_35), false));
var __VLS_33;
var __VLS_38 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
    field: "desc",
    label: "客户端描述",
}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
        field: "desc",
        label: "客户端描述",
    }], __VLS_functionalComponentArgsRest(__VLS_39), false));
__VLS_41.slots.default;
var __VLS_42 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    modelValue: (__VLS_ctx.clientBasicInfoForm.desc),
    placeholder: "请输入客户端描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.clientBasicInfoForm.desc),
        placeholder: "请输入客户端描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
var __VLS_41;
var __VLS_46 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    hideLabel: true,
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
__VLS_49.slots.default;
var __VLS_50 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_51), false));
__VLS_53.slots.default;
var __VLS_54 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
__VLS_57.slots.default;
var __VLS_57;
var __VLS_58 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58(__assign({ 'onClick': {} })));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_59), false));
var __VLS_62;
var __VLS_63;
var __VLS_64;
var __VLS_65 = {
    onClick: (__VLS_ctx.handleResetClientBasicInfoForm)
};
__VLS_61.slots.default;
var __VLS_61;
var __VLS_53;
var __VLS_49;
var __VLS_23;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_66 = {}.ADescriptions;
/** @type {[typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
    column: (2),
    align: ({ label: 'right' }),
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        column: (2),
        align: ({ label: 'right' }),
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
__VLS_69.slots.default;
var __VLS_70 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
    label: "Client ID",
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        label: "Client ID",
    }], __VLS_functionalComponentArgsRest(__VLS_71), false));
__VLS_73.slots.default;
var __VLS_74 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    text: (__VLS_ctx.clientEndpointInfo.id),
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.clientEndpointInfo.id),
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
var __VLS_73;
var __VLS_78 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
    label: "Client Secret",
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        label: "Client Secret",
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_82 = {}.APopconfirm;
/** @type {[typeof __VLS_components.APopconfirm, typeof __VLS_components.aPopconfirm, typeof __VLS_components.APopconfirm, typeof __VLS_components.aPopconfirm, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82(__assign({ 'onOk': {} }, { content: "刷新后，原密钥将失效，确定刷新吗？" })));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([__assign({ 'onOk': {} }, { content: "刷新后，原密钥将失效，确定刷新吗？" })], __VLS_functionalComponentArgsRest(__VLS_83), false));
var __VLS_86;
var __VLS_87;
var __VLS_88;
var __VLS_89 = {
    onOk: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleUpdateClientSecretSubmit(__VLS_ctx.clientEndpointInfo.id);
    }
};
__VLS_85.slots.default;
var __VLS_90 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
    type: "text",
}));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
        type: "text",
    }], __VLS_functionalComponentArgsRest(__VLS_91), false));
__VLS_93.slots.default;
{
    var __VLS_thisSlot = __VLS_93.slots.icon;
    var __VLS_94 = {}.IconRefresh;
    /** @type {[typeof __VLS_components.IconRefresh, typeof __VLS_components.iconRefresh, ]} */ ;
    // @ts-ignore
    var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({}));
    var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_95), false));
}
var __VLS_93;
var __VLS_85;
var __VLS_81;
var __VLS_98 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
    label: "Issuer",
}));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
        label: "Issuer",
    }], __VLS_functionalComponentArgsRest(__VLS_99), false));
__VLS_101.slots.default;
(__VLS_ctx.clientEndpointInfo.issuer);
var __VLS_101;
var __VLS_102 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
    label: "OpenID Connect URL",
}));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
        label: "OpenID Connect URL",
    }], __VLS_functionalComponentArgsRest(__VLS_103), false));
__VLS_105.slots.default;
var __VLS_106 = {}.ALink;
/** @type {[typeof __VLS_components.ALink, typeof __VLS_components.aLink, typeof __VLS_components.ALink, typeof __VLS_components.aLink, ]} */ ;
// @ts-ignore
var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({}));
var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_107), false));
__VLS_109.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.a, __VLS_intrinsicElements.a)({
    href: (__VLS_ctx.clientEndpointInfo.openidConfiguration),
    target: "_blank",
});
(__VLS_ctx.clientEndpointInfo.openidConfiguration);
var __VLS_110 = {}.IconLaunch;
/** @type {[typeof __VLS_components.IconLaunch, typeof __VLS_components.iconLaunch, ]} */ ;
// @ts-ignore
var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110(__assign({ style: {} })));
var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_111), false));
var __VLS_109;
var __VLS_105;
var __VLS_114 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
    label: "JWKS 公钥端点",
}));
var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
        label: "JWKS 公钥端点",
    }], __VLS_functionalComponentArgsRest(__VLS_115), false));
__VLS_117.slots.default;
var __VLS_118 = {}.ALink;
/** @type {[typeof __VLS_components.ALink, typeof __VLS_components.aLink, typeof __VLS_components.ALink, typeof __VLS_components.aLink, ]} */ ;
// @ts-ignore
var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({}));
var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_119), false));
__VLS_121.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.a, __VLS_intrinsicElements.a)({
    href: (__VLS_ctx.clientEndpointInfo.jwks),
    target: "_blank",
});
(__VLS_ctx.clientEndpointInfo.jwks);
var __VLS_122 = {}.IconLaunch;
/** @type {[typeof __VLS_components.IconLaunch, typeof __VLS_components.iconLaunch, ]} */ ;
// @ts-ignore
var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122(__assign({ style: {} })));
var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_123), false));
var __VLS_121;
var __VLS_117;
var __VLS_126 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126({
    label: "认证端点",
}));
var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([{
        label: "认证端点",
    }], __VLS_functionalComponentArgsRest(__VLS_127), false));
__VLS_129.slots.default;
(__VLS_ctx.clientEndpointInfo.authorize);
var __VLS_129;
var __VLS_130 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130({
    label: "Token 端点",
}));
var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{
        label: "Token 端点",
    }], __VLS_functionalComponentArgsRest(__VLS_131), false));
__VLS_133.slots.default;
(__VLS_ctx.clientEndpointInfo.token);
var __VLS_133;
var __VLS_134 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134({
    label: "用户信息端点",
}));
var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([{
        label: "用户信息端点",
    }], __VLS_functionalComponentArgsRest(__VLS_135), false));
__VLS_137.slots.default;
(__VLS_ctx.clientEndpointInfo.userinfo);
var __VLS_137;
var __VLS_69;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_138 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_139 = __VLS_asFunctionalComponent(__VLS_138, new __VLS_138(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.clientAuthInfoForm), rules: (__VLS_ctx.clientAuthInfoFormRules), ref: "clientAuthInfoFormRef", layout: "vertical" })));
var __VLS_140 = __VLS_139.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.clientAuthInfoForm), rules: (__VLS_ctx.clientAuthInfoFormRules), ref: "clientAuthInfoFormRef", layout: "vertical" })], __VLS_functionalComponentArgsRest(__VLS_139), false));
var __VLS_142;
var __VLS_143;
var __VLS_144;
var __VLS_145 = {
    onSubmitSuccess: (__VLS_ctx.handleClientAuthInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.clientAuthInfoFormRef} */ ;
var __VLS_146 = {};
__VLS_141.slots.default;
var __VLS_148 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
    field: "redirectUri",
    label: "登录回调 URL",
}));
var __VLS_150 = __VLS_149.apply(void 0, __spreadArray([{
        field: "redirectUri",
        label: "登录回调 URL",
    }], __VLS_functionalComponentArgsRest(__VLS_149), false));
__VLS_151.slots.default;
var __VLS_152 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152({
    modelValue: (__VLS_ctx.clientAuthInfoForm.redirectUri),
    placeholder: "请输入登录回调 URL",
}));
var __VLS_154 = __VLS_153.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.clientAuthInfoForm.redirectUri),
        placeholder: "请输入登录回调 URL",
    }], __VLS_functionalComponentArgsRest(__VLS_153), false));
var __VLS_151;
var __VLS_156 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156({
    hideLabel: true,
}));
var __VLS_158 = __VLS_157.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_157), false));
__VLS_159.slots.default;
var __VLS_160 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({}));
var __VLS_162 = __VLS_161.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_161), false));
__VLS_163.slots.default;
var __VLS_164 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_166 = __VLS_165.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_165), false));
__VLS_167.slots.default;
var __VLS_167;
var __VLS_168 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168(__assign({ 'onClick': {} })));
var __VLS_170 = __VLS_169.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_169), false));
var __VLS_172;
var __VLS_173;
var __VLS_174;
var __VLS_175 = {
    onClick: (__VLS_ctx.handleResetClientAuthInfoForm)
};
__VLS_171.slots.default;
var __VLS_171;
var __VLS_163;
var __VLS_159;
var __VLS_141;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "delete-container" }));
var __VLS_176 = {}.AAlert;
/** @type {[typeof __VLS_components.AAlert, typeof __VLS_components.aAlert, typeof __VLS_components.AAlert, typeof __VLS_components.aAlert, ]} */ ;
// @ts-ignore
var __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176({
    type: "warning",
}));
var __VLS_178 = __VLS_177.apply(void 0, __spreadArray([{
        type: "warning",
    }], __VLS_functionalComponentArgsRest(__VLS_177), false));
__VLS_179.slots.default;
{
    var __VLS_thisSlot = __VLS_179.slots.title;
    (__VLS_ctx.clientName);
}
{
    var __VLS_thisSlot = __VLS_179.slots.action;
    var __VLS_180 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180(__assign({ 'onClick': {} }, { type: "primary", status: "danger" })));
    var __VLS_182 = __VLS_181.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary", status: "danger" })], __VLS_functionalComponentArgsRest(__VLS_181), false));
    var __VLS_184 = void 0;
    var __VLS_185 = void 0;
    var __VLS_186 = void 0;
    var __VLS_187 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleDeleteClientSubmit(__VLS_ctx.clientName, __VLS_ctx.clientEndpointInfo.id);
        }
    };
    __VLS_183.slots.default;
    var __VLS_183;
}
var __VLS_179;
var __VLS_19;
var __VLS_188 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_189 = __VLS_asFunctionalComponent(__VLS_188, new __VLS_188({
    key: "oidc_setting",
    title: "协议配置",
}));
var __VLS_190 = __VLS_189.apply(void 0, __spreadArray([{
        key: "oidc_setting",
        title: "协议配置",
    }], __VLS_functionalComponentArgsRest(__VLS_189), false));
__VLS_191.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_192 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192(__assign({ 'onSubmitSuccess': {} }, { layout: "vertical", model: (__VLS_ctx.clientAuthorizeInfoForm), rules: (__VLS_ctx.clientAuthorizeInfoFormRules), ref: "clientAuthorizeInfoFormRef" })));
var __VLS_194 = __VLS_193.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { layout: "vertical", model: (__VLS_ctx.clientAuthorizeInfoForm), rules: (__VLS_ctx.clientAuthorizeInfoFormRules), ref: "clientAuthorizeInfoFormRef" })], __VLS_functionalComponentArgsRest(__VLS_193), false));
var __VLS_196;
var __VLS_197;
var __VLS_198;
var __VLS_199 = {
    onSubmitSuccess: (__VLS_ctx.handleClientAuthorizeInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.clientAuthorizeInfoFormRef} */ ;
var __VLS_200 = {};
__VLS_195.slots.default;
var __VLS_202 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_203 = __VLS_asFunctionalComponent(__VLS_202, new __VLS_202({
    field: "grantTypes",
    label: "授权模式",
}));
var __VLS_204 = __VLS_203.apply(void 0, __spreadArray([{
        field: "grantTypes",
        label: "授权模式",
    }], __VLS_functionalComponentArgsRest(__VLS_203), false));
__VLS_205.slots.default;
var __VLS_206 = {}.ACheckboxGroup;
/** @type {[typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, ]} */ ;
// @ts-ignore
var __VLS_207 = __VLS_asFunctionalComponent(__VLS_206, new __VLS_206({
    modelValue: (__VLS_ctx.clientAuthorizeInfoForm.grantTypes),
}));
var __VLS_208 = __VLS_207.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.clientAuthorizeInfoForm.grantTypes),
    }], __VLS_functionalComponentArgsRest(__VLS_207), false));
__VLS_209.slots.default;
var __VLS_210 = {}.ACheckbox;
/** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
// @ts-ignore
var __VLS_211 = __VLS_asFunctionalComponent(__VLS_210, new __VLS_210({
    value: "authorization_code",
}));
var __VLS_212 = __VLS_211.apply(void 0, __spreadArray([{
        value: "authorization_code",
    }], __VLS_functionalComponentArgsRest(__VLS_211), false));
__VLS_213.slots.default;
var __VLS_213;
var __VLS_214 = {}.ACheckbox;
/** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
// @ts-ignore
var __VLS_215 = __VLS_asFunctionalComponent(__VLS_214, new __VLS_214({
    value: "refresh_token",
}));
var __VLS_216 = __VLS_215.apply(void 0, __spreadArray([{
        value: "refresh_token",
    }], __VLS_functionalComponentArgsRest(__VLS_215), false));
__VLS_217.slots.default;
var __VLS_217;
var __VLS_218 = {}.ACheckbox;
/** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
// @ts-ignore
var __VLS_219 = __VLS_asFunctionalComponent(__VLS_218, new __VLS_218({
    value: "client_credentials",
}));
var __VLS_220 = __VLS_219.apply(void 0, __spreadArray([{
        value: "client_credentials",
    }], __VLS_functionalComponentArgsRest(__VLS_219), false));
__VLS_221.slots.default;
var __VLS_221;
var __VLS_222 = {}.ACheckbox;
/** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
// @ts-ignore
var __VLS_223 = __VLS_asFunctionalComponent(__VLS_222, new __VLS_222({
    value: "password",
}));
var __VLS_224 = __VLS_223.apply(void 0, __spreadArray([{
        value: "password",
    }], __VLS_functionalComponentArgsRest(__VLS_223), false));
__VLS_225.slots.default;
var __VLS_225;
var __VLS_209;
var __VLS_205;
var __VLS_226 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_227 = __VLS_asFunctionalComponent(__VLS_226, new __VLS_226({
    field: "authenticationMethods",
    label: "客户端认证方式",
}));
var __VLS_228 = __VLS_227.apply(void 0, __spreadArray([{
        field: "authenticationMethods",
        label: "客户端认证方式",
    }], __VLS_functionalComponentArgsRest(__VLS_227), false));
__VLS_229.slots.default;
var __VLS_230 = {}.ACheckboxGroup;
/** @type {[typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, ]} */ ;
// @ts-ignore
var __VLS_231 = __VLS_asFunctionalComponent(__VLS_230, new __VLS_230({
    modelValue: (__VLS_ctx.clientAuthorizeInfoForm.authenticationMethods),
}));
var __VLS_232 = __VLS_231.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.clientAuthorizeInfoForm.authenticationMethods),
    }], __VLS_functionalComponentArgsRest(__VLS_231), false));
__VLS_233.slots.default;
var __VLS_234 = {}.ACheckbox;
/** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
// @ts-ignore
var __VLS_235 = __VLS_asFunctionalComponent(__VLS_234, new __VLS_234({
    value: "client_secret_basic",
}));
var __VLS_236 = __VLS_235.apply(void 0, __spreadArray([{
        value: "client_secret_basic",
    }], __VLS_functionalComponentArgsRest(__VLS_235), false));
__VLS_237.slots.default;
var __VLS_237;
var __VLS_238 = {}.ACheckbox;
/** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
// @ts-ignore
var __VLS_239 = __VLS_asFunctionalComponent(__VLS_238, new __VLS_238({
    value: "client_secret_post",
}));
var __VLS_240 = __VLS_239.apply(void 0, __spreadArray([{
        value: "client_secret_post",
    }], __VLS_functionalComponentArgsRest(__VLS_239), false));
__VLS_241.slots.default;
var __VLS_241;
var __VLS_242 = {}.ACheckbox;
/** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
// @ts-ignore
var __VLS_243 = __VLS_asFunctionalComponent(__VLS_242, new __VLS_242({
    value: "none",
}));
var __VLS_244 = __VLS_243.apply(void 0, __spreadArray([{
        value: "none",
    }], __VLS_functionalComponentArgsRest(__VLS_243), false));
__VLS_245.slots.default;
var __VLS_245;
var __VLS_233;
var __VLS_229;
var __VLS_246 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_247 = __VLS_asFunctionalComponent(__VLS_246, new __VLS_246({
    field: "requireProofKey",
    label: "是否需要 PKCE",
}));
var __VLS_248 = __VLS_247.apply(void 0, __spreadArray([{
        field: "requireProofKey",
        label: "是否需要 PKCE",
    }], __VLS_functionalComponentArgsRest(__VLS_247), false));
__VLS_249.slots.default;
var __VLS_250 = {}.ASwitch;
/** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
// @ts-ignore
var __VLS_251 = __VLS_asFunctionalComponent(__VLS_250, new __VLS_250({
    type: "round",
    modelValue: (__VLS_ctx.clientAuthorizeInfoForm.requireProofKey),
}));
var __VLS_252 = __VLS_251.apply(void 0, __spreadArray([{
        type: "round",
        modelValue: (__VLS_ctx.clientAuthorizeInfoForm.requireProofKey),
    }], __VLS_functionalComponentArgsRest(__VLS_251), false));
var __VLS_249;
var __VLS_254 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_255 = __VLS_asFunctionalComponent(__VLS_254, new __VLS_254({
    gutter: (24),
}));
var __VLS_256 = __VLS_255.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_255), false));
__VLS_257.slots.default;
var __VLS_258 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_259 = __VLS_asFunctionalComponent(__VLS_258, new __VLS_258({
    span: (12),
}));
var __VLS_260 = __VLS_259.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_259), false));
__VLS_261.slots.default;
var __VLS_262 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_263 = __VLS_asFunctionalComponent(__VLS_262, new __VLS_262({
    field: "authorizationCodeTimeToLive",
    label: "授权码过期时间",
}));
var __VLS_264 = __VLS_263.apply(void 0, __spreadArray([{
        field: "authorizationCodeTimeToLive",
        label: "授权码过期时间",
    }], __VLS_functionalComponentArgsRest(__VLS_263), false));
__VLS_265.slots.default;
var __VLS_266 = {}.AInputGroup;
/** @type {[typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, ]} */ ;
// @ts-ignore
var __VLS_267 = __VLS_asFunctionalComponent(__VLS_266, new __VLS_266({}));
var __VLS_268 = __VLS_267.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_267), false));
__VLS_269.slots.default;
var __VLS_270 = {}.AInputNumber;
/** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
// @ts-ignore
var __VLS_271 = __VLS_asFunctionalComponent(__VLS_270, new __VLS_270({
    modelValue: (__VLS_ctx.clientAuthorizeInfoForm.authorizationCodeTimeToLive),
    min: (1),
}));
var __VLS_272 = __VLS_271.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.clientAuthorizeInfoForm.authorizationCodeTimeToLive),
        min: (1),
    }], __VLS_functionalComponentArgsRest(__VLS_271), false));
var __VLS_274 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_275 = __VLS_asFunctionalComponent(__VLS_274, new __VLS_274(__assign({ modelValue: (__VLS_ctx.authorizationCodeTimeToLiveUnit) }, { style: ({ width: '120px' }) })));
var __VLS_276 = __VLS_275.apply(void 0, __spreadArray([__assign({ modelValue: (__VLS_ctx.authorizationCodeTimeToLiveUnit) }, { style: ({ width: '120px' }) })], __VLS_functionalComponentArgsRest(__VLS_275), false));
__VLS_277.slots.default;
var __VLS_278 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_279 = __VLS_asFunctionalComponent(__VLS_278, new __VLS_278({
    value: (1),
}));
var __VLS_280 = __VLS_279.apply(void 0, __spreadArray([{
        value: (1),
    }], __VLS_functionalComponentArgsRest(__VLS_279), false));
__VLS_281.slots.default;
var __VLS_281;
var __VLS_282 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_283 = __VLS_asFunctionalComponent(__VLS_282, new __VLS_282({
    value: (60),
}));
var __VLS_284 = __VLS_283.apply(void 0, __spreadArray([{
        value: (60),
    }], __VLS_functionalComponentArgsRest(__VLS_283), false));
__VLS_285.slots.default;
var __VLS_285;
var __VLS_286 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_287 = __VLS_asFunctionalComponent(__VLS_286, new __VLS_286({
    value: (1440),
}));
var __VLS_288 = __VLS_287.apply(void 0, __spreadArray([{
        value: (1440),
    }], __VLS_functionalComponentArgsRest(__VLS_287), false));
__VLS_289.slots.default;
var __VLS_289;
var __VLS_277;
var __VLS_269;
var __VLS_265;
var __VLS_261;
var __VLS_290 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_291 = __VLS_asFunctionalComponent(__VLS_290, new __VLS_290({
    span: (12),
}));
var __VLS_292 = __VLS_291.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_291), false));
__VLS_293.slots.default;
var __VLS_294 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_295 = __VLS_asFunctionalComponent(__VLS_294, new __VLS_294({
    field: "accessTokenTimeToLive",
    label: "访问令牌过期时间",
}));
var __VLS_296 = __VLS_295.apply(void 0, __spreadArray([{
        field: "accessTokenTimeToLive",
        label: "访问令牌过期时间",
    }], __VLS_functionalComponentArgsRest(__VLS_295), false));
__VLS_297.slots.default;
var __VLS_298 = {}.AInputGroup;
/** @type {[typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, ]} */ ;
// @ts-ignore
var __VLS_299 = __VLS_asFunctionalComponent(__VLS_298, new __VLS_298({}));
var __VLS_300 = __VLS_299.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_299), false));
__VLS_301.slots.default;
var __VLS_302 = {}.AInputNumber;
/** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
// @ts-ignore
var __VLS_303 = __VLS_asFunctionalComponent(__VLS_302, new __VLS_302({
    modelValue: (__VLS_ctx.clientAuthorizeInfoForm.accessTokenTimeToLive),
    min: (1),
}));
var __VLS_304 = __VLS_303.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.clientAuthorizeInfoForm.accessTokenTimeToLive),
        min: (1),
    }], __VLS_functionalComponentArgsRest(__VLS_303), false));
var __VLS_306 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_307 = __VLS_asFunctionalComponent(__VLS_306, new __VLS_306(__assign({ modelValue: (__VLS_ctx.accessTokenTimeToLiveUnit) }, { style: ({ width: '120px' }) })));
var __VLS_308 = __VLS_307.apply(void 0, __spreadArray([__assign({ modelValue: (__VLS_ctx.accessTokenTimeToLiveUnit) }, { style: ({ width: '120px' }) })], __VLS_functionalComponentArgsRest(__VLS_307), false));
__VLS_309.slots.default;
var __VLS_310 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_311 = __VLS_asFunctionalComponent(__VLS_310, new __VLS_310({
    value: (1),
}));
var __VLS_312 = __VLS_311.apply(void 0, __spreadArray([{
        value: (1),
    }], __VLS_functionalComponentArgsRest(__VLS_311), false));
__VLS_313.slots.default;
var __VLS_313;
var __VLS_314 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_315 = __VLS_asFunctionalComponent(__VLS_314, new __VLS_314({
    value: (60),
}));
var __VLS_316 = __VLS_315.apply(void 0, __spreadArray([{
        value: (60),
    }], __VLS_functionalComponentArgsRest(__VLS_315), false));
__VLS_317.slots.default;
var __VLS_317;
var __VLS_318 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_319 = __VLS_asFunctionalComponent(__VLS_318, new __VLS_318({
    value: (1440),
}));
var __VLS_320 = __VLS_319.apply(void 0, __spreadArray([{
        value: (1440),
    }], __VLS_functionalComponentArgsRest(__VLS_319), false));
__VLS_321.slots.default;
var __VLS_321;
var __VLS_309;
var __VLS_301;
var __VLS_297;
var __VLS_293;
var __VLS_257;
var __VLS_322 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_323 = __VLS_asFunctionalComponent(__VLS_322, new __VLS_322({
    field: "refreshTokenTimeToLive",
    label: "刷新令牌过期时间",
}));
var __VLS_324 = __VLS_323.apply(void 0, __spreadArray([{
        field: "refreshTokenTimeToLive",
        label: "刷新令牌过期时间",
    }], __VLS_functionalComponentArgsRest(__VLS_323), false));
__VLS_325.slots.default;
var __VLS_326 = {}.AInputGroup;
/** @type {[typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, typeof __VLS_components.AInputGroup, typeof __VLS_components.aInputGroup, ]} */ ;
// @ts-ignore
var __VLS_327 = __VLS_asFunctionalComponent(__VLS_326, new __VLS_326({}));
var __VLS_328 = __VLS_327.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_327), false));
__VLS_329.slots.default;
var __VLS_330 = {}.AInputNumber;
/** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
// @ts-ignore
var __VLS_331 = __VLS_asFunctionalComponent(__VLS_330, new __VLS_330({
    modelValue: (__VLS_ctx.clientAuthorizeInfoForm.refreshTokenTimeToLive),
    min: (1),
}));
var __VLS_332 = __VLS_331.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.clientAuthorizeInfoForm.refreshTokenTimeToLive),
        min: (1),
    }], __VLS_functionalComponentArgsRest(__VLS_331), false));
var __VLS_334 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_335 = __VLS_asFunctionalComponent(__VLS_334, new __VLS_334(__assign({ modelValue: (__VLS_ctx.refreshTokenTimeToLiveUnit) }, { style: ({ width: '120px' }) })));
var __VLS_336 = __VLS_335.apply(void 0, __spreadArray([__assign({ modelValue: (__VLS_ctx.refreshTokenTimeToLiveUnit) }, { style: ({ width: '120px' }) })], __VLS_functionalComponentArgsRest(__VLS_335), false));
__VLS_337.slots.default;
var __VLS_338 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_339 = __VLS_asFunctionalComponent(__VLS_338, new __VLS_338({
    value: (1),
}));
var __VLS_340 = __VLS_339.apply(void 0, __spreadArray([{
        value: (1),
    }], __VLS_functionalComponentArgsRest(__VLS_339), false));
__VLS_341.slots.default;
var __VLS_341;
var __VLS_342 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_343 = __VLS_asFunctionalComponent(__VLS_342, new __VLS_342({
    value: (60),
}));
var __VLS_344 = __VLS_343.apply(void 0, __spreadArray([{
        value: (60),
    }], __VLS_functionalComponentArgsRest(__VLS_343), false));
__VLS_345.slots.default;
var __VLS_345;
var __VLS_346 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_347 = __VLS_asFunctionalComponent(__VLS_346, new __VLS_346({
    value: (1440),
}));
var __VLS_348 = __VLS_347.apply(void 0, __spreadArray([{
        value: (1440),
    }], __VLS_functionalComponentArgsRest(__VLS_347), false));
__VLS_349.slots.default;
var __VLS_349;
var __VLS_337;
var __VLS_329;
var __VLS_325;
var __VLS_350 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_351 = __VLS_asFunctionalComponent(__VLS_350, new __VLS_350({
    label: "OIDC scope",
}));
var __VLS_352 = __VLS_351.apply(void 0, __spreadArray([{
        label: "OIDC scope",
    }], __VLS_functionalComponentArgsRest(__VLS_351), false));
__VLS_353.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "oidc-scope" }));
var __VLS_354 = {}.ACheckboxGroup;
/** @type {[typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, ]} */ ;
// @ts-ignore
var __VLS_355 = __VLS_asFunctionalComponent(__VLS_354, new __VLS_354({
    direction: "vertical",
    modelValue: (__VLS_ctx.clientAuthorizeInfoForm.scopes),
}));
var __VLS_356 = __VLS_355.apply(void 0, __spreadArray([{
        direction: "vertical",
        modelValue: (__VLS_ctx.clientAuthorizeInfoForm.scopes),
    }], __VLS_functionalComponentArgsRest(__VLS_355), false));
__VLS_357.slots.default;
var _loop_1 = function (scope, index) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "scope-container" }, { key: (scope.name) }));
    var __VLS_358 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_359 = __VLS_asFunctionalComponent(__VLS_358, new __VLS_358({
        value: (scope.name),
    }));
    var __VLS_360 = __VLS_359.apply(void 0, __spreadArray([{
            value: (scope.name),
        }], __VLS_functionalComponentArgsRest(__VLS_359), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "scope-content" }));
    var __VLS_362 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_363 = __VLS_asFunctionalComponent(__VLS_362, new __VLS_362({
        gutter: (24),
        align: "center",
    }));
    var __VLS_364 = __VLS_363.apply(void 0, __spreadArray([{
            gutter: (24),
            align: "center",
        }], __VLS_functionalComponentArgsRest(__VLS_363), false));
    __VLS_365.slots.default;
    var __VLS_366 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_367 = __VLS_asFunctionalComponent(__VLS_366, new __VLS_366({
        span: (8),
    }));
    var __VLS_368 = __VLS_367.apply(void 0, __spreadArray([{
            span: (8),
        }], __VLS_functionalComponentArgsRest(__VLS_367), false));
    __VLS_369.slots.default;
    var __VLS_370 = {}.InputText;
    /** @type {[typeof __VLS_components.InputText, typeof __VLS_components.inputText, ]} */ ;
    // @ts-ignore
    var __VLS_371 = __VLS_asFunctionalComponent(__VLS_370, new __VLS_370({
        modelValue: (scope.name),
        placeholder: "请输入 Scope 名称",
    }));
    var __VLS_372 = __VLS_371.apply(void 0, __spreadArray([{
            modelValue: (scope.name),
            placeholder: "请输入 Scope 名称",
        }], __VLS_functionalComponentArgsRest(__VLS_371), false));
    var __VLS_374 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_375 = __VLS_asFunctionalComponent(__VLS_374, new __VLS_374({
        span: (12),
    }));
    var __VLS_376 = __VLS_375.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_375), false));
    __VLS_377.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "scope-claim" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
    var __VLS_378 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_379 = __VLS_asFunctionalComponent(__VLS_378, new __VLS_378({
        placeholder: "请选择 OIDC claim",
        multiple: true,
        modelValue: (scope.claims),
    }));
    var __VLS_380 = __VLS_379.apply(void 0, __spreadArray([{
            placeholder: "请选择 OIDC claim",
            multiple: true,
            modelValue: (scope.claims),
        }], __VLS_functionalComponentArgsRest(__VLS_379), false));
    __VLS_381.slots.default;
    for (var _f = 0, _g = __VLS_getVForSourceType((__VLS_ctx.oidcClaims)); _f < _g.length; _f++) {
        var _h = _g[_f], claim = _h[0], index_1 = _h[1];
        var __VLS_382 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_383 = __VLS_asFunctionalComponent(__VLS_382, new __VLS_382({
            key: (claim.name),
            value: (claim.id),
        }));
        var __VLS_384 = __VLS_383.apply(void 0, __spreadArray([{
                key: (claim.name),
                value: (claim.id),
            }], __VLS_functionalComponentArgsRest(__VLS_383), false));
        __VLS_385.slots.default;
        (claim.name);
    }
    var __VLS_386 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_387 = __VLS_asFunctionalComponent(__VLS_386, new __VLS_386({
        span: (4),
    }));
    var __VLS_388 = __VLS_387.apply(void 0, __spreadArray([{
            span: (4),
        }], __VLS_functionalComponentArgsRest(__VLS_387), false));
    __VLS_389.slots.default;
    var __VLS_390 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_391 = __VLS_asFunctionalComponent(__VLS_390, new __VLS_390({}));
    var __VLS_392 = __VLS_391.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_391), false));
    __VLS_393.slots.default;
    var __VLS_394 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_395 = __VLS_asFunctionalComponent(__VLS_394, new __VLS_394(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
    var __VLS_396 = __VLS_395.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_395), false));
    var __VLS_398 = void 0;
    var __VLS_399 = void 0;
    var __VLS_400 = void 0;
    var __VLS_401 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleSaveOidcScopeSubmit(scope);
        }
    };
    __VLS_397.slots.default;
    {
        var __VLS_thisSlot = __VLS_397.slots.icon;
        var __VLS_402 = {}.IconSave;
        /** @type {[typeof __VLS_components.IconSave, typeof __VLS_components.iconSave, ]} */ ;
        // @ts-ignore
        var __VLS_403 = __VLS_asFunctionalComponent(__VLS_402, new __VLS_402({}));
        var __VLS_404 = __VLS_403.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_403), false));
    }
    if (scope.id) {
        var __VLS_406 = {}.APopconfirm;
        /** @type {[typeof __VLS_components.APopconfirm, typeof __VLS_components.aPopconfirm, typeof __VLS_components.APopconfirm, typeof __VLS_components.aPopconfirm, ]} */ ;
        // @ts-ignore
        var __VLS_407 = __VLS_asFunctionalComponent(__VLS_406, new __VLS_406(__assign({ 'onOk': {} }, { content: "删除后，所有客户端将失去该 scope 的访问权限，确定删除吗？", okButtonProps: ({ status: 'danger' }) })));
        var __VLS_408 = __VLS_407.apply(void 0, __spreadArray([__assign({ 'onOk': {} }, { content: "删除后，所有客户端将失去该 scope 的访问权限，确定删除吗？", okButtonProps: ({ status: 'danger' }) })], __VLS_functionalComponentArgsRest(__VLS_407), false));
        var __VLS_410 = void 0;
        var __VLS_411 = void 0;
        var __VLS_412 = void 0;
        var __VLS_413 = {
            onOk: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!(scope.id))
                    return;
                __VLS_ctx.handleDeleteOidcScopeSubmit(scope);
            }
        };
        __VLS_409.slots.default;
        var __VLS_414 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_415 = __VLS_asFunctionalComponent(__VLS_414, new __VLS_414({
            type: "text",
            size: "mini",
            status: "danger",
        }));
        var __VLS_416 = __VLS_415.apply(void 0, __spreadArray([{
                type: "text",
                size: "mini",
                status: "danger",
            }], __VLS_functionalComponentArgsRest(__VLS_415), false));
        __VLS_417.slots.default;
        {
            var __VLS_thisSlot = __VLS_417.slots.icon;
            var __VLS_418 = {}.IconDelete;
            /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
            // @ts-ignore
            var __VLS_419 = __VLS_asFunctionalComponent(__VLS_418, new __VLS_418({}));
            var __VLS_420 = __VLS_419.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_419), false));
        }
    }
    else {
        var __VLS_422 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_423 = __VLS_asFunctionalComponent(__VLS_422, new __VLS_422(__assign({ 'onClick': {} }, { type: "text", size: "mini", status: "danger" })));
        var __VLS_424 = __VLS_423.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini", status: "danger" })], __VLS_functionalComponentArgsRest(__VLS_423), false));
        var __VLS_426 = void 0;
        var __VLS_427 = void 0;
        var __VLS_428 = void 0;
        var __VLS_429 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!!(scope.id))
                    return;
                __VLS_ctx.handleRemoveOidcScope(scope);
            }
        };
        __VLS_425.slots.default;
        {
            var __VLS_thisSlot = __VLS_425.slots.icon;
            var __VLS_430 = {}.IconMinusCircle;
            /** @type {[typeof __VLS_components.IconMinusCircle, typeof __VLS_components.iconMinusCircle, ]} */ ;
            // @ts-ignore
            var __VLS_431 = __VLS_asFunctionalComponent(__VLS_430, new __VLS_430({}));
            var __VLS_432 = __VLS_431.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_431), false));
        }
    }
};
var __VLS_369, __VLS_385, __VLS_381, __VLS_377, __VLS_397, __VLS_417, __VLS_409, __VLS_425, __VLS_393, __VLS_389, __VLS_365;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.oidcScopes)); _i < _a.length; _i++) {
    var _b = _a[_i], scope = _b[0], index = _b[1];
    _loop_1(scope, index);
}
var __VLS_357;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "btn-container" }));
var __VLS_434 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_435 = __VLS_asFunctionalComponent(__VLS_434, new __VLS_434(__assign({ 'onClick': {} }, { type: "text" })));
var __VLS_436 = __VLS_435.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text" })], __VLS_functionalComponentArgsRest(__VLS_435), false));
var __VLS_438;
var __VLS_439;
var __VLS_440;
var __VLS_441 = {
    onClick: (__VLS_ctx.handleCreateOidcScope)
};
__VLS_437.slots.default;
{
    var __VLS_thisSlot = __VLS_437.slots.icon;
    var __VLS_442 = {}.IconPlus;
    /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
    // @ts-ignore
    var __VLS_443 = __VLS_asFunctionalComponent(__VLS_442, new __VLS_442({}));
    var __VLS_444 = __VLS_443.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_443), false));
}
var __VLS_437;
var __VLS_353;
var __VLS_446 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_447 = __VLS_asFunctionalComponent(__VLS_446, new __VLS_446({
    hideLabel: true,
}));
var __VLS_448 = __VLS_447.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_447), false));
__VLS_449.slots.default;
var __VLS_450 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_451 = __VLS_asFunctionalComponent(__VLS_450, new __VLS_450({}));
var __VLS_452 = __VLS_451.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_451), false));
__VLS_453.slots.default;
var __VLS_454 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_455 = __VLS_asFunctionalComponent(__VLS_454, new __VLS_454({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_456 = __VLS_455.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_455), false));
__VLS_457.slots.default;
var __VLS_457;
var __VLS_458 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_459 = __VLS_asFunctionalComponent(__VLS_458, new __VLS_458(__assign({ 'onClick': {} })));
var __VLS_460 = __VLS_459.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_459), false));
var __VLS_462;
var __VLS_463;
var __VLS_464;
var __VLS_465 = {
    onClick: (__VLS_ctx.handleResetClientAuthorizeInfoForm)
};
__VLS_461.slots.default;
var __VLS_461;
var __VLS_453;
var __VLS_449;
var __VLS_195;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var _loop_2 = function (claim, index) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "claim-container" }, { key: (claim.id) }));
    var __VLS_466 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_467 = __VLS_asFunctionalComponent(__VLS_466, new __VLS_466(__assign({ gutter: (24), align: "center" }, { style: {} })));
    var __VLS_468 = __VLS_467.apply(void 0, __spreadArray([__assign({ gutter: (24), align: "center" }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_467), false));
    __VLS_469.slots.default;
    var __VLS_470 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_471 = __VLS_asFunctionalComponent(__VLS_470, new __VLS_470({
        span: (8),
    }));
    var __VLS_472 = __VLS_471.apply(void 0, __spreadArray([{
            span: (8),
        }], __VLS_functionalComponentArgsRest(__VLS_471), false));
    __VLS_473.slots.default;
    var __VLS_474 = {}.InputText;
    /** @type {[typeof __VLS_components.InputText, typeof __VLS_components.inputText, ]} */ ;
    // @ts-ignore
    var __VLS_475 = __VLS_asFunctionalComponent(__VLS_474, new __VLS_474({
        modelValue: (claim.name),
        placeholder: "请输入 Claim 名称",
    }));
    var __VLS_476 = __VLS_475.apply(void 0, __spreadArray([{
            modelValue: (claim.name),
            placeholder: "请输入 Claim 名称",
        }], __VLS_functionalComponentArgsRest(__VLS_475), false));
    var __VLS_478 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_479 = __VLS_asFunctionalComponent(__VLS_478, new __VLS_478({
        span: (12),
    }));
    var __VLS_480 = __VLS_479.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_479), false));
    __VLS_481.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "claim-attr" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
    var __VLS_482 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_483 = __VLS_asFunctionalComponent(__VLS_482, new __VLS_482({
        placeholder: "请选择用户字段",
        modelValue: (claim.userAttrId),
    }));
    var __VLS_484 = __VLS_483.apply(void 0, __spreadArray([{
            placeholder: "请选择用户字段",
            modelValue: (claim.userAttrId),
        }], __VLS_functionalComponentArgsRest(__VLS_483), false));
    __VLS_485.slots.default;
    for (var _j = 0, _k = __VLS_getVForSourceType((__VLS_ctx.userAttrs)); _j < _k.length; _j++) {
        var _l = _k[_j], userAttr = _l[0], index_2 = _l[1];
        var __VLS_486 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_487 = __VLS_asFunctionalComponent(__VLS_486, new __VLS_486({
            key: (userAttr.id),
            value: (userAttr.id),
        }));
        var __VLS_488 = __VLS_487.apply(void 0, __spreadArray([{
                key: (userAttr.id),
                value: (userAttr.id),
            }], __VLS_functionalComponentArgsRest(__VLS_487), false));
        __VLS_489.slots.default;
        (userAttr.name);
        (userAttr.key);
    }
    var __VLS_490 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_491 = __VLS_asFunctionalComponent(__VLS_490, new __VLS_490({
        span: (4),
    }));
    var __VLS_492 = __VLS_491.apply(void 0, __spreadArray([{
            span: (4),
        }], __VLS_functionalComponentArgsRest(__VLS_491), false));
    __VLS_493.slots.default;
    var __VLS_494 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_495 = __VLS_asFunctionalComponent(__VLS_494, new __VLS_494({}));
    var __VLS_496 = __VLS_495.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_495), false));
    __VLS_497.slots.default;
    var __VLS_498 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_499 = __VLS_asFunctionalComponent(__VLS_498, new __VLS_498(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
    var __VLS_500 = __VLS_499.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_499), false));
    var __VLS_502 = void 0;
    var __VLS_503 = void 0;
    var __VLS_504 = void 0;
    var __VLS_505 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleSaveOidcClaimSubmit(claim);
        }
    };
    __VLS_501.slots.default;
    {
        var __VLS_thisSlot = __VLS_501.slots.icon;
        var __VLS_506 = {}.IconSave;
        /** @type {[typeof __VLS_components.IconSave, typeof __VLS_components.iconSave, ]} */ ;
        // @ts-ignore
        var __VLS_507 = __VLS_asFunctionalComponent(__VLS_506, new __VLS_506({}));
        var __VLS_508 = __VLS_507.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_507), false));
    }
    if (claim.id) {
        var __VLS_510 = {}.APopconfirm;
        /** @type {[typeof __VLS_components.APopconfirm, typeof __VLS_components.aPopconfirm, typeof __VLS_components.APopconfirm, typeof __VLS_components.aPopconfirm, ]} */ ;
        // @ts-ignore
        var __VLS_511 = __VLS_asFunctionalComponent(__VLS_510, new __VLS_510(__assign({ 'onOk': {} }, { content: "删除后，该 claim 将从 scope 中移除，确定删除吗？", okButtonProps: ({ status: 'danger' }) })));
        var __VLS_512 = __VLS_511.apply(void 0, __spreadArray([__assign({ 'onOk': {} }, { content: "删除后，该 claim 将从 scope 中移除，确定删除吗？", okButtonProps: ({ status: 'danger' }) })], __VLS_functionalComponentArgsRest(__VLS_511), false));
        var __VLS_514 = void 0;
        var __VLS_515 = void 0;
        var __VLS_516 = void 0;
        var __VLS_517 = {
            onOk: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!(claim.id))
                    return;
                __VLS_ctx.handleDeleteOidcClaimSubmit(claim);
            }
        };
        __VLS_513.slots.default;
        var __VLS_518 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_519 = __VLS_asFunctionalComponent(__VLS_518, new __VLS_518({
            type: "text",
            size: "mini",
            status: "danger",
        }));
        var __VLS_520 = __VLS_519.apply(void 0, __spreadArray([{
                type: "text",
                size: "mini",
                status: "danger",
            }], __VLS_functionalComponentArgsRest(__VLS_519), false));
        __VLS_521.slots.default;
        {
            var __VLS_thisSlot = __VLS_521.slots.icon;
            var __VLS_522 = {}.IconDelete;
            /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
            // @ts-ignore
            var __VLS_523 = __VLS_asFunctionalComponent(__VLS_522, new __VLS_522({}));
            var __VLS_524 = __VLS_523.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_523), false));
        }
    }
    else {
        var __VLS_526 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_527 = __VLS_asFunctionalComponent(__VLS_526, new __VLS_526(__assign({ 'onClick': {} }, { type: "text", size: "mini", status: "danger" })));
        var __VLS_528 = __VLS_527.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini", status: "danger" })], __VLS_functionalComponentArgsRest(__VLS_527), false));
        var __VLS_530 = void 0;
        var __VLS_531 = void 0;
        var __VLS_532 = void 0;
        var __VLS_533 = {
            onClick: (__VLS_ctx.handleRemoveOidcClaim)
        };
        __VLS_529.slots.default;
        {
            var __VLS_thisSlot = __VLS_529.slots.icon;
            var __VLS_534 = {}.IconMinusCircle;
            /** @type {[typeof __VLS_components.IconMinusCircle, typeof __VLS_components.iconMinusCircle, ]} */ ;
            // @ts-ignore
            var __VLS_535 = __VLS_asFunctionalComponent(__VLS_534, new __VLS_534({}));
            var __VLS_536 = __VLS_535.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_535), false));
        }
    }
};
var __VLS_473, __VLS_489, __VLS_485, __VLS_481, __VLS_501, __VLS_521, __VLS_513, __VLS_529, __VLS_497, __VLS_493, __VLS_469;
for (var _c = 0, _d = __VLS_getVForSourceType((__VLS_ctx.claimMappings)); _c < _d.length; _c++) {
    var _e = _d[_c], claim = _e[0], index = _e[1];
    _loop_2(claim, index);
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "btn-container" }));
var __VLS_538 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_539 = __VLS_asFunctionalComponent(__VLS_538, new __VLS_538(__assign({ 'onClick': {} }, { type: "text" })));
var __VLS_540 = __VLS_539.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text" })], __VLS_functionalComponentArgsRest(__VLS_539), false));
var __VLS_542;
var __VLS_543;
var __VLS_544;
var __VLS_545 = {
    onClick: (__VLS_ctx.handleCreateOidcClaim)
};
__VLS_541.slots.default;
{
    var __VLS_thisSlot = __VLS_541.slots.icon;
    var __VLS_546 = {}.IconPlus;
    /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
    // @ts-ignore
    var __VLS_547 = __VLS_asFunctionalComponent(__VLS_546, new __VLS_546({}));
    var __VLS_548 = __VLS_547.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_547), false));
}
var __VLS_541;
var __VLS_191;
var __VLS_11;
var __VLS_3;
var __VLS_550 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_551 = __VLS_asFunctionalComponent(__VLS_550, new __VLS_550({
    visible: (__VLS_ctx.updateClientSecretSuccessModalVisible),
    hideCancel: true,
    footer: (false),
    maskClosable: (false),
    width: (660),
}));
var __VLS_552 = __VLS_551.apply(void 0, __spreadArray([{
        visible: (__VLS_ctx.updateClientSecretSuccessModalVisible),
        hideCancel: true,
        footer: (false),
        maskClosable: (false),
        width: (660),
    }], __VLS_functionalComponentArgsRest(__VLS_551), false));
__VLS_553.slots.default;
{
    var __VLS_thisSlot = __VLS_553.slots.title;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_554 = {}.IconCheckCircleFill;
    /** @type {[typeof __VLS_components.IconCheckCircleFill, typeof __VLS_components.iconCheckCircleFill, ]} */ ;
    // @ts-ignore
    var __VLS_555 = __VLS_asFunctionalComponent(__VLS_554, new __VLS_554(__assign({ style: {} })));
    var __VLS_556 = __VLS_555.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_555), false));
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "secret-modal" }));
var __VLS_558 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_559 = __VLS_asFunctionalComponent(__VLS_558, new __VLS_558({
    text: (__VLS_ctx.newClientSecret),
}));
var __VLS_560 = __VLS_559.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.newClientSecret),
    }], __VLS_functionalComponentArgsRest(__VLS_559), false));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-text" }));
var __VLS_553;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['delete-container']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['oidc-scope']} */ ;
/** @type {__VLS_StyleScopedClasses['scope-container']} */ ;
/** @type {__VLS_StyleScopedClasses['scope-content']} */ ;
/** @type {__VLS_StyleScopedClasses['scope-claim']} */ ;
/** @type {__VLS_StyleScopedClasses['btn-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['claim-container']} */ ;
/** @type {__VLS_StyleScopedClasses['claim-attr']} */ ;
/** @type {__VLS_StyleScopedClasses['btn-container']} */ ;
/** @type {__VLS_StyleScopedClasses['secret-modal']} */ ;
/** @type {__VLS_StyleScopedClasses['info-text']} */ ;
// @ts-ignore
var __VLS_29 = __VLS_28, __VLS_147 = __VLS_146, __VLS_201 = __VLS_200;
var __VLS_dollars;
var __VLS_self;
