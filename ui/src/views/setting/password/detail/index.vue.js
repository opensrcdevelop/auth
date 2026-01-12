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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "detail-title" }));
(__VLS_ctx.passwordPolicyName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_8 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    model: (__VLS_ctx.basicInfoForm),
    rules: (__VLS_ctx.basicInfoFormRules),
    ref: "basicInfoFormRef",
    layout: "vertical",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.basicInfoForm),
        rules: (__VLS_ctx.basicInfoFormRules),
        ref: "basicInfoFormRef",
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
/** @type {typeof __VLS_ctx.basicInfoFormRef} */ ;
var __VLS_12 = {};
__VLS_11.slots.default;
var __VLS_14 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_15 = __VLS_asFunctionalComponent(__VLS_14, new __VLS_14({
    field: "name",
    label: "策略名称",
}));
var __VLS_16 = __VLS_15.apply(void 0, __spreadArray([{
        field: "name",
        label: "策略名称",
    }], __VLS_functionalComponentArgsRest(__VLS_15), false));
__VLS_17.slots.default;
var __VLS_18 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18({
    modelValue: (__VLS_ctx.basicInfoForm.name),
    placeholder: "请输入策略名称",
    disabled: (__VLS_ctx.passwordPolicyName === '默认策略'),
}));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.basicInfoForm.name),
        placeholder: "请输入策略名称",
        disabled: (__VLS_ctx.passwordPolicyName === '默认策略'),
    }], __VLS_functionalComponentArgsRest(__VLS_19), false));
var __VLS_17;
var __VLS_22 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_23 = __VLS_asFunctionalComponent(__VLS_22, new __VLS_22({
    field: "desc",
    label: "策略描述",
}));
var __VLS_24 = __VLS_23.apply(void 0, __spreadArray([{
        field: "desc",
        label: "策略描述",
    }], __VLS_functionalComponentArgsRest(__VLS_23), false));
__VLS_25.slots.default;
var __VLS_26 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26({
    modelValue: (__VLS_ctx.basicInfoForm.desc),
    placeholder: "请输入策略描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.basicInfoForm.desc),
        placeholder: "请输入策略描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_27), false));
var __VLS_25;
var __VLS_11;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
if (__VLS_ctx.passwordPolicyName === '默认策略') {
    var __VLS_30 = {}.AAlert;
    /** @type {[typeof __VLS_components.AAlert, typeof __VLS_components.aAlert, typeof __VLS_components.AAlert, typeof __VLS_components.aAlert, ]} */ ;
    // @ts-ignore
    var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30(__assign({ showIcon: (false) }, { style: {} })));
    var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([__assign({ showIcon: (false) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_31), false));
    __VLS_33.slots.default;
    {
        var __VLS_thisSlot = __VLS_33.slots.title;
    }
    var __VLS_33;
}
else {
    var __VLS_34 = {}.AForm;
    /** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
    // @ts-ignore
    var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
        model: (__VLS_ctx.policyPrincipalForm),
        ref: "policyPrincipalFormRef",
        rules: (__VLS_ctx.policyPrincipalFormRules),
        layout: "vertical",
    }));
    var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
            model: (__VLS_ctx.policyPrincipalForm),
            ref: "policyPrincipalFormRef",
            rules: (__VLS_ctx.policyPrincipalFormRules),
            layout: "vertical",
        }], __VLS_functionalComponentArgsRest(__VLS_35), false));
    /** @type {typeof __VLS_ctx.policyPrincipalFormRef} */ ;
    var __VLS_38 = {};
    __VLS_37.slots.default;
    var __VLS_40 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
        gutter: (24),
    }));
    var __VLS_42 = __VLS_41.apply(void 0, __spreadArray([{
            gutter: (24),
        }], __VLS_functionalComponentArgsRest(__VLS_41), false));
    __VLS_43.slots.default;
    var __VLS_44 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
        span: (12),
    }));
    var __VLS_46 = __VLS_45.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_45), false));
    __VLS_47.slots.default;
    var __VLS_48 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
        field: "type",
        label: "主体类型",
    }));
    var __VLS_50 = __VLS_49.apply(void 0, __spreadArray([{
            field: "type",
            label: "主体类型",
        }], __VLS_functionalComponentArgsRest(__VLS_49), false));
    __VLS_51.slots.default;
    var __VLS_52 = {}.ARadioGroup;
    /** @type {[typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, ]} */ ;
    // @ts-ignore
    var __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52(__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.policyPrincipalForm.type) })));
    var __VLS_54 = __VLS_53.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.policyPrincipalForm.type) })], __VLS_functionalComponentArgsRest(__VLS_53), false));
    var __VLS_56 = void 0;
    var __VLS_57 = void 0;
    var __VLS_58 = void 0;
    var __VLS_59 = {
        onChange: (__VLS_ctx.principalSelectChange)
    };
    __VLS_55.slots.default;
    var __VLS_60 = {}.ARadio;
    /** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
    // @ts-ignore
    var __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
        value: "USER",
    }));
    var __VLS_62 = __VLS_61.apply(void 0, __spreadArray([{
            value: "USER",
        }], __VLS_functionalComponentArgsRest(__VLS_61), false));
    __VLS_63.slots.default;
    var __VLS_63;
    var __VLS_64 = {}.ARadio;
    /** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
    // @ts-ignore
    var __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
        value: "USER_GROUP",
    }));
    var __VLS_66 = __VLS_65.apply(void 0, __spreadArray([{
            value: "USER_GROUP",
        }], __VLS_functionalComponentArgsRest(__VLS_65), false));
    __VLS_67.slots.default;
    var __VLS_67;
    var __VLS_55;
    var __VLS_51;
    var __VLS_47;
    var __VLS_68 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
        span: (12),
    }));
    var __VLS_70 = __VLS_69.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_69), false));
    __VLS_71.slots.default;
    var __VLS_72 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
        field: "id",
        label: "主体",
    }));
    var __VLS_74 = __VLS_73.apply(void 0, __spreadArray([{
            field: "id",
            label: "主体",
        }], __VLS_functionalComponentArgsRest(__VLS_73), false));
    __VLS_75.slots.default;
    if (__VLS_ctx.policyPrincipalForm.type === 'USER') {
        var __VLS_76 = {}.ASelect;
        /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
        // @ts-ignore
        var __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { placeholder: "请选择用户", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.policyPrincipalForm.id), inputValue: (__VLS_ctx.userSearchKeyword), filterOption: (false) })));
        var __VLS_78 = __VLS_77.apply(void 0, __spreadArray([__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { placeholder: "请选择用户", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.policyPrincipalForm.id), inputValue: (__VLS_ctx.userSearchKeyword), filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_77), false));
        var __VLS_80 = void 0;
        var __VLS_81 = void 0;
        var __VLS_82 = void 0;
        var __VLS_83 = {
            onSearch: (__VLS_ctx.handleGetAllUsers)
        };
        var __VLS_84 = {
            onClear: (__VLS_ctx.handleGetAllUsers)
        };
        __VLS_79.slots.default;
        for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.userList)); _i < _a.length; _i++) {
            var user = _a[_i][0];
            var __VLS_85 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_86 = __VLS_asFunctionalComponent(__VLS_85, new __VLS_85({
                key: (user.userId),
                value: (user.userId),
            }));
            var __VLS_87 = __VLS_86.apply(void 0, __spreadArray([{
                    key: (user.userId),
                    value: (user.userId),
                }], __VLS_functionalComponentArgsRest(__VLS_86), false));
            __VLS_88.slots.default;
            (user.username);
            var __VLS_88;
        }
        var __VLS_79;
    }
    if (__VLS_ctx.policyPrincipalForm.type === 'USER_GROUP') {
        var __VLS_89 = {}.ASelect;
        /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
        // @ts-ignore
        var __VLS_90 = __VLS_asFunctionalComponent(__VLS_89, new __VLS_89(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { placeholder: "请选择用户组", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.policyPrincipalForm.id), inputValue: (__VLS_ctx.userGroupSearchKeyword), filterOption: (false) })));
        var __VLS_91 = __VLS_90.apply(void 0, __spreadArray([__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { placeholder: "请选择用户组", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.policyPrincipalForm.id), inputValue: (__VLS_ctx.userGroupSearchKeyword), filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_90), false));
        var __VLS_93 = void 0;
        var __VLS_94 = void 0;
        var __VLS_95 = void 0;
        var __VLS_96 = {
            onSearch: (__VLS_ctx.handleGetAllUserGroups)
        };
        var __VLS_97 = {
            onClear: (__VLS_ctx.handleGetAllUserGroups)
        };
        __VLS_92.slots.default;
        for (var _b = 0, _c = __VLS_getVForSourceType((__VLS_ctx.userGroupList)); _b < _c.length; _b++) {
            var userGroup = _c[_b][0];
            var __VLS_98 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
                key: (userGroup.id),
                value: (userGroup.id),
            }));
            var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
                    key: (userGroup.id),
                    value: (userGroup.id),
                }], __VLS_functionalComponentArgsRest(__VLS_99), false));
            __VLS_101.slots.default;
            (userGroup.name);
            var __VLS_101;
        }
        var __VLS_92;
    }
    var __VLS_75;
    var __VLS_71;
    var __VLS_43;
    var __VLS_37;
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_102 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
    model: (__VLS_ctx.passwordStrengthForm),
    ref: "passwordStrengthFormRef",
    rules: (__VLS_ctx.passwordStrengthFormRules),
    layout: "vertical",
}));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.passwordStrengthForm),
        ref: "passwordStrengthFormRef",
        rules: (__VLS_ctx.passwordStrengthFormRules),
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_103), false));
/** @type {typeof __VLS_ctx.passwordStrengthFormRef} */ ;
var __VLS_106 = {};
__VLS_105.slots.default;
var __VLS_108 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
    field: "passwordStrength",
}));
var __VLS_110 = __VLS_109.apply(void 0, __spreadArray([{
        field: "passwordStrength",
    }], __VLS_functionalComponentArgsRest(__VLS_109), false));
__VLS_111.slots.default;
{
    var __VLS_thisSlot = __VLS_111.slots.label;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "password-strength-label" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    var __VLS_112 = {}.ATag;
    /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
    // @ts-ignore
    var __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112(__assign({ class: "password-strength-tag" }, { color: "gray", size: "small" })));
    var __VLS_114 = __VLS_113.apply(void 0, __spreadArray([__assign({ class: "password-strength-tag" }, { color: "gray", size: "small" })], __VLS_functionalComponentArgsRest(__VLS_113), false));
    __VLS_115.slots.default;
    (__VLS_ctx.passwordStrengthLabel);
    var __VLS_115;
}
var __VLS_116 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116(__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.passwordStrengthForm.passwordStrength), placeholder: "请选择密码规则" })));
var __VLS_118 = __VLS_117.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.passwordStrengthForm.passwordStrength), placeholder: "请选择密码规则" })], __VLS_functionalComponentArgsRest(__VLS_117), false));
var __VLS_120;
var __VLS_121;
var __VLS_122;
var __VLS_123 = {
    onChange: (__VLS_ctx.handleCheckPassword)
};
__VLS_119.slots.default;
var __VLS_124 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({
    value: (0),
}));
var __VLS_126 = __VLS_125.apply(void 0, __spreadArray([{
        value: (0),
    }], __VLS_functionalComponentArgsRest(__VLS_125), false));
__VLS_127.slots.default;
var __VLS_127;
var __VLS_128 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_129 = __VLS_asFunctionalComponent(__VLS_128, new __VLS_128({
    value: (1),
}));
var __VLS_130 = __VLS_129.apply(void 0, __spreadArray([{
        value: (1),
    }], __VLS_functionalComponentArgsRest(__VLS_129), false));
__VLS_131.slots.default;
var __VLS_131;
var __VLS_132 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
    value: (2),
}));
var __VLS_134 = __VLS_133.apply(void 0, __spreadArray([{
        value: (2),
    }], __VLS_functionalComponentArgsRest(__VLS_133), false));
__VLS_135.slots.default;
var __VLS_135;
var __VLS_136 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
    value: (3),
}));
var __VLS_138 = __VLS_137.apply(void 0, __spreadArray([{
        value: (3),
    }], __VLS_functionalComponentArgsRest(__VLS_137), false));
__VLS_139.slots.default;
var __VLS_139;
var __VLS_140 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({
    value: (4),
}));
var __VLS_142 = __VLS_141.apply(void 0, __spreadArray([{
        value: (4),
    }], __VLS_functionalComponentArgsRest(__VLS_141), false));
__VLS_143.slots.default;
var __VLS_143;
var __VLS_119;
var __VLS_111;
if (__VLS_ctx.passwordStrengthForm.passwordStrength === 4) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_144 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
        gutter: (24),
    }));
    var __VLS_146 = __VLS_145.apply(void 0, __spreadArray([{
            gutter: (24),
        }], __VLS_functionalComponentArgsRest(__VLS_145), false));
    __VLS_147.slots.default;
    var __VLS_148 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({
        span: (4),
    }));
    var __VLS_150 = __VLS_149.apply(void 0, __spreadArray([{
            span: (4),
        }], __VLS_functionalComponentArgsRest(__VLS_149), false));
    __VLS_151.slots.default;
    var __VLS_152 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152({
        field: "minLength",
        label: "密码最小位数",
    }));
    var __VLS_154 = __VLS_153.apply(void 0, __spreadArray([{
            field: "minLength",
            label: "密码最小位数",
        }], __VLS_functionalComponentArgsRest(__VLS_153), false));
    __VLS_155.slots.default;
    var __VLS_156 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156({
        step: (1),
        min: (1),
        max: (35),
        mode: "button",
        modelValue: (__VLS_ctx.passwordStrengthForm.minLength),
    }));
    var __VLS_158 = __VLS_157.apply(void 0, __spreadArray([{
            step: (1),
            min: (1),
            max: (35),
            mode: "button",
            modelValue: (__VLS_ctx.passwordStrengthForm.minLength),
        }], __VLS_functionalComponentArgsRest(__VLS_157), false));
    var __VLS_155;
    var __VLS_151;
    var __VLS_160 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({
        span: (4),
    }));
    var __VLS_162 = __VLS_161.apply(void 0, __spreadArray([{
            span: (4),
        }], __VLS_functionalComponentArgsRest(__VLS_161), false));
    __VLS_163.slots.default;
    var __VLS_164 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
        field: "maxLength",
        label: "密码最大位数",
    }));
    var __VLS_166 = __VLS_165.apply(void 0, __spreadArray([{
            field: "maxLength",
            label: "密码最大位数",
        }], __VLS_functionalComponentArgsRest(__VLS_165), false));
    __VLS_167.slots.default;
    var __VLS_168 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
        step: (1),
        min: (1),
        max: (35),
        mode: "button",
        modelValue: (__VLS_ctx.passwordStrengthForm.maxLength),
    }));
    var __VLS_170 = __VLS_169.apply(void 0, __spreadArray([{
            step: (1),
            min: (1),
            max: (35),
            mode: "button",
            modelValue: (__VLS_ctx.passwordStrengthForm.maxLength),
        }], __VLS_functionalComponentArgsRest(__VLS_169), false));
    var __VLS_167;
    var __VLS_163;
    var __VLS_172 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172({
        span: (16),
    }));
    var __VLS_174 = __VLS_173.apply(void 0, __spreadArray([{
            span: (16),
        }], __VLS_functionalComponentArgsRest(__VLS_173), false));
    __VLS_175.slots.default;
    var __VLS_176 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176({
        field: "charType",
        label: "密码字符类型要求",
    }));
    var __VLS_178 = __VLS_177.apply(void 0, __spreadArray([{
            field: "charType",
            label: "密码字符类型要求",
        }], __VLS_functionalComponentArgsRest(__VLS_177), false));
    __VLS_179.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-char-type-container" }));
    var __VLS_180 = {}.ACheckboxGroup;
    /** @type {[typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, ]} */ ;
    // @ts-ignore
    var __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180(__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.passwordStrengthForm.charType) })));
    var __VLS_182 = __VLS_181.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.passwordStrengthForm.charType) })], __VLS_functionalComponentArgsRest(__VLS_181), false));
    var __VLS_184 = void 0;
    var __VLS_185 = void 0;
    var __VLS_186 = void 0;
    var __VLS_187 = {
        onChange: (function () { return (__VLS_ctx.passwordStrengthForm.minCharTypeCount = 0); })
    };
    __VLS_183.slots.default;
    var __VLS_188 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_189 = __VLS_asFunctionalComponent(__VLS_188, new __VLS_188({
        value: "UPPER_CASE",
    }));
    var __VLS_190 = __VLS_189.apply(void 0, __spreadArray([{
            value: "UPPER_CASE",
        }], __VLS_functionalComponentArgsRest(__VLS_189), false));
    __VLS_191.slots.default;
    var __VLS_191;
    var __VLS_192 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192({
        value: "LOWER_CASE",
    }));
    var __VLS_194 = __VLS_193.apply(void 0, __spreadArray([{
            value: "LOWER_CASE",
        }], __VLS_functionalComponentArgsRest(__VLS_193), false));
    __VLS_195.slots.default;
    var __VLS_195;
    var __VLS_196 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196({
        value: "NUMBER",
    }));
    var __VLS_198 = __VLS_197.apply(void 0, __spreadArray([{
            value: "NUMBER",
        }], __VLS_functionalComponentArgsRest(__VLS_197), false));
    __VLS_199.slots.default;
    var __VLS_199;
    var __VLS_200 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_201 = __VLS_asFunctionalComponent(__VLS_200, new __VLS_200({
        value: "SPECIAL_CHAR",
    }));
    var __VLS_202 = __VLS_201.apply(void 0, __spreadArray([{
            value: "SPECIAL_CHAR",
        }], __VLS_functionalComponentArgsRest(__VLS_201), false));
    __VLS_203.slots.default;
    var __VLS_203;
    var __VLS_183;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "chat-type-count-select" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_204 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204(__assign(__assign({ modelValue: (__VLS_ctx.passwordStrengthForm.minCharTypeCount) }, { style: {} }), { disabled: (__VLS_ctx.passwordStrengthForm.charType.length === 0) })));
    var __VLS_206 = __VLS_205.apply(void 0, __spreadArray([__assign(__assign({ modelValue: (__VLS_ctx.passwordStrengthForm.minCharTypeCount) }, { style: {} }), { disabled: (__VLS_ctx.passwordStrengthForm.charType.length === 0) })], __VLS_functionalComponentArgsRest(__VLS_205), false));
    __VLS_207.slots.default;
    if (__VLS_ctx.passwordStrengthForm.charType.length === 0 ||
        __VLS_ctx.passwordStrengthForm.charType.length === 1) {
        var __VLS_208 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
            value: (0),
        }));
        var __VLS_210 = __VLS_209.apply(void 0, __spreadArray([{
                value: (0),
            }], __VLS_functionalComponentArgsRest(__VLS_209), false));
        __VLS_211.slots.default;
        var __VLS_211;
    }
    if (__VLS_ctx.passwordStrengthForm.charType.length > 1) {
        for (var _d = 0, _e = __VLS_getVForSourceType((__VLS_ctx.passwordStrengthForm.charType)); _d < _e.length; _d++) {
            var _f = _e[_d], item = _f[0], i = _f[1];
            var __VLS_212 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_213 = __VLS_asFunctionalComponent(__VLS_212, new __VLS_212({
                key: (i),
                value: (i),
            }));
            var __VLS_214 = __VLS_213.apply(void 0, __spreadArray([{
                    key: (i),
                    value: (i),
                }], __VLS_functionalComponentArgsRest(__VLS_213), false));
            __VLS_215.slots.default;
            (i === 0 ? "全部" : "".concat(i, " \u79CD"));
            var __VLS_215;
        }
    }
    var __VLS_207;
    var __VLS_179;
    var __VLS_175;
    var __VLS_147;
    var __VLS_216 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_217 = __VLS_asFunctionalComponent(__VLS_216, new __VLS_216({
        label: "禁止使用的密码类型配置",
    }));
    var __VLS_218 = __VLS_217.apply(void 0, __spreadArray([{
            label: "禁止使用的密码类型配置",
        }], __VLS_functionalComponentArgsRest(__VLS_217), false));
    __VLS_219.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "prohibit-password-type-container" }));
    var __VLS_220 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_221 = __VLS_asFunctionalComponent(__VLS_220, new __VLS_220(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitUserInfo) })));
    var __VLS_222 = __VLS_221.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitUserInfo) })], __VLS_functionalComponentArgsRest(__VLS_221), false));
    __VLS_223.slots.default;
    var __VLS_223;
    var __VLS_224 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_225 = __VLS_asFunctionalComponent(__VLS_224, new __VLS_224(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitSingleChar) })));
    var __VLS_226 = __VLS_225.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitSingleChar) })], __VLS_functionalComponentArgsRest(__VLS_225), false));
    __VLS_227.slots.default;
    var __VLS_227;
    var __VLS_228 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitConsecutiveChar) })));
    var __VLS_230 = __VLS_229.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitConsecutiveChar) })], __VLS_functionalComponentArgsRest(__VLS_229), false));
    __VLS_231.slots.default;
    var __VLS_231;
    var __VLS_232 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_233 = __VLS_asFunctionalComponent(__VLS_232, new __VLS_232(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitContainConsecutiveChar) })));
    var __VLS_234 = __VLS_233.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitContainConsecutiveChar) })], __VLS_functionalComponentArgsRest(__VLS_233), false));
    __VLS_235.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_236 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_237 = __VLS_asFunctionalComponent(__VLS_236, new __VLS_236(__assign({ style: {} }, { min: (2), max: (35), step: (1), mode: "button", size: "mini", modelValue: (__VLS_ctx.passwordStrengthForm.minConsecutiveCharLength), disabled: (!__VLS_ctx.passwordStrengthForm.prohibitContainConsecutiveChar) })));
    var __VLS_238 = __VLS_237.apply(void 0, __spreadArray([__assign({ style: {} }, { min: (2), max: (35), step: (1), mode: "button", size: "mini", modelValue: (__VLS_ctx.passwordStrengthForm.minConsecutiveCharLength), disabled: (!__VLS_ctx.passwordStrengthForm.prohibitContainConsecutiveChar) })], __VLS_functionalComponentArgsRest(__VLS_237), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_235;
    var __VLS_240 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_241 = __VLS_asFunctionalComponent(__VLS_240, new __VLS_240(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitContainRepeatChar) })));
    var __VLS_242 = __VLS_241.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitContainRepeatChar) })], __VLS_functionalComponentArgsRest(__VLS_241), false));
    __VLS_243.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_244 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244(__assign({ style: {} }, { min: (2), max: (35), step: (1), mode: "button", size: "mini", modelValue: (__VLS_ctx.passwordStrengthForm.minRepeatCharLength), disabled: (!__VLS_ctx.passwordStrengthForm.prohibitContainRepeatChar) })));
    var __VLS_246 = __VLS_245.apply(void 0, __spreadArray([__assign({ style: {} }, { min: (2), max: (35), step: (1), mode: "button", size: "mini", modelValue: (__VLS_ctx.passwordStrengthForm.minRepeatCharLength), disabled: (!__VLS_ctx.passwordStrengthForm.prohibitContainRepeatChar) })], __VLS_functionalComponentArgsRest(__VLS_245), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_243;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
    var __VLS_248 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_249 = __VLS_asFunctionalComponent(__VLS_248, new __VLS_248({
        modelValue: (__VLS_ctx.passwordStrengthForm.prohibitSpecificPassword),
    }));
    var __VLS_250 = __VLS_249.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.passwordStrengthForm.prohibitSpecificPassword),
        }], __VLS_functionalComponentArgsRest(__VLS_249), false));
    __VLS_251.slots.default;
    var __VLS_251;
    var __VLS_252 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_253 = __VLS_asFunctionalComponent(__VLS_252, new __VLS_252(__assign(__assign({ 'onClick': {} }, { type: "text", size: "mini" }), { style: {} })));
    var __VLS_254 = __VLS_253.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { type: "text", size: "mini" }), { style: {} })], __VLS_functionalComponentArgsRest(__VLS_253), false));
    var __VLS_256 = void 0;
    var __VLS_257 = void 0;
    var __VLS_258 = void 0;
    var __VLS_259 = {
        onClick: (function () { return (__VLS_ctx.specificPasswordListEditModalVisible = true); })
    };
    __VLS_255.slots.default;
    var __VLS_255;
    var __VLS_219;
}
var __VLS_260 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_261 = __VLS_asFunctionalComponent(__VLS_260, new __VLS_260({
    label: "检查密码",
}));
var __VLS_262 = __VLS_261.apply(void 0, __spreadArray([{
        label: "检查密码",
    }], __VLS_functionalComponentArgsRest(__VLS_261), false));
__VLS_263.slots.default;
var __VLS_264 = {}.PasswordChecker;
/** @type {[typeof __VLS_components.PasswordChecker, typeof __VLS_components.passwordChecker, ]} */ ;
// @ts-ignore
var __VLS_265 = __VLS_asFunctionalComponent(__VLS_264, new __VLS_264(__assign({ 'onCheck': {} }, { ref: "passwordCheckerRef", type: "text", loading: (__VLS_ctx.checkLoading), checkRes: (__VLS_ctx.checkRes) })));
var __VLS_266 = __VLS_265.apply(void 0, __spreadArray([__assign({ 'onCheck': {} }, { ref: "passwordCheckerRef", type: "text", loading: (__VLS_ctx.checkLoading), checkRes: (__VLS_ctx.checkRes) })], __VLS_functionalComponentArgsRest(__VLS_265), false));
var __VLS_268;
var __VLS_269;
var __VLS_270;
var __VLS_271 = {
    onCheck: (__VLS_ctx.handleCheckPassword)
};
/** @type {typeof __VLS_ctx.passwordCheckerRef} */ ;
var __VLS_272 = {};
var __VLS_267;
var __VLS_263;
var __VLS_274 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_275 = __VLS_asFunctionalComponent(__VLS_274, new __VLS_274({
    label: "用户登录密码强度检查",
}));
var __VLS_276 = __VLS_275.apply(void 0, __spreadArray([{
        label: "用户登录密码强度检查",
    }], __VLS_functionalComponentArgsRest(__VLS_275), false));
__VLS_277.slots.default;
var __VLS_278 = {}.ASwitch;
/** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
// @ts-ignore
var __VLS_279 = __VLS_asFunctionalComponent(__VLS_278, new __VLS_278({
    type: "round",
    modelValue: (__VLS_ctx.passwordStrengthForm.enablePasswordDetection),
}));
var __VLS_280 = __VLS_279.apply(void 0, __spreadArray([{
        type: "round",
        modelValue: (__VLS_ctx.passwordStrengthForm.enablePasswordDetection),
    }], __VLS_functionalComponentArgsRest(__VLS_279), false));
{
    var __VLS_thisSlot = __VLS_277.slots.extra;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
}
var __VLS_277;
var __VLS_105;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_282 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_283 = __VLS_asFunctionalComponent(__VLS_282, new __VLS_282({
    model: (__VLS_ctx.forceChangePasswordForm),
    ref: "forceChangePasswordFormRef",
    rules: (__VLS_ctx.forceChangePasswordFormRules),
    layout: "vertical",
}));
var __VLS_284 = __VLS_283.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.forceChangePasswordForm),
        ref: "forceChangePasswordFormRef",
        rules: (__VLS_ctx.forceChangePasswordFormRules),
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_283), false));
/** @type {typeof __VLS_ctx.forceChangePasswordFormRef} */ ;
var __VLS_286 = {};
__VLS_285.slots.default;
var __VLS_288 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_289 = __VLS_asFunctionalComponent(__VLS_288, new __VLS_288({
    label: "强制用户定期修改密码",
}));
var __VLS_290 = __VLS_289.apply(void 0, __spreadArray([{
        label: "强制用户定期修改密码",
    }], __VLS_functionalComponentArgsRest(__VLS_289), false));
__VLS_291.slots.default;
var __VLS_292 = {}.ASwitch;
/** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
// @ts-ignore
var __VLS_293 = __VLS_asFunctionalComponent(__VLS_292, new __VLS_292({
    type: "round",
    modelValue: (__VLS_ctx.forceChangePasswordForm.enableForceChangePassword),
}));
var __VLS_294 = __VLS_293.apply(void 0, __spreadArray([{
        type: "round",
        modelValue: (__VLS_ctx.forceChangePasswordForm.enableForceChangePassword),
    }], __VLS_functionalComponentArgsRest(__VLS_293), false));
{
    var __VLS_thisSlot = __VLS_291.slots.extra;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
}
var __VLS_291;
if (__VLS_ctx.forceChangePasswordForm.enableForceChangePassword) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_296 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_297 = __VLS_asFunctionalComponent(__VLS_296, new __VLS_296({
        gutter: (24),
    }));
    var __VLS_298 = __VLS_297.apply(void 0, __spreadArray([{
            gutter: (24),
        }], __VLS_functionalComponentArgsRest(__VLS_297), false));
    __VLS_299.slots.default;
    var __VLS_300 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_301 = __VLS_asFunctionalComponent(__VLS_300, new __VLS_300({
        span: (12),
    }));
    var __VLS_302 = __VLS_301.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_301), false));
    __VLS_303.slots.default;
    var __VLS_304 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_305 = __VLS_asFunctionalComponent(__VLS_304, new __VLS_304({
        field: "forcedCycle",
        label: "强制修改密码周期",
    }));
    var __VLS_306 = __VLS_305.apply(void 0, __spreadArray([{
            field: "forcedCycle",
            label: "强制修改密码周期",
        }], __VLS_functionalComponentArgsRest(__VLS_305), false));
    __VLS_307.slots.default;
    var __VLS_308 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_309 = __VLS_asFunctionalComponent(__VLS_308, new __VLS_308(__assign({ min: (1), max: (99), step: (1), mode: "button", modelValue: (__VLS_ctx.forceChangePasswordForm.forcedCycle) }, { style: {} })));
    var __VLS_310 = __VLS_309.apply(void 0, __spreadArray([__assign({ min: (1), max: (99), step: (1), mode: "button", modelValue: (__VLS_ctx.forceChangePasswordForm.forcedCycle) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_309), false));
    var __VLS_312 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_313 = __VLS_asFunctionalComponent(__VLS_312, new __VLS_312(__assign({ modelValue: (__VLS_ctx.forceChangePasswordForm.forcedCycleUnit) }, { style: {} })));
    var __VLS_314 = __VLS_313.apply(void 0, __spreadArray([__assign({ modelValue: (__VLS_ctx.forceChangePasswordForm.forcedCycleUnit) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_313), false));
    __VLS_315.slots.default;
    var __VLS_316 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_317 = __VLS_asFunctionalComponent(__VLS_316, new __VLS_316({
        value: "DAY",
    }));
    var __VLS_318 = __VLS_317.apply(void 0, __spreadArray([{
            value: "DAY",
        }], __VLS_functionalComponentArgsRest(__VLS_317), false));
    __VLS_319.slots.default;
    var __VLS_319;
    var __VLS_320 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_321 = __VLS_asFunctionalComponent(__VLS_320, new __VLS_320({
        value: "MONTH",
    }));
    var __VLS_322 = __VLS_321.apply(void 0, __spreadArray([{
            value: "MONTH",
        }], __VLS_functionalComponentArgsRest(__VLS_321), false));
    __VLS_323.slots.default;
    var __VLS_323;
    var __VLS_324 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_325 = __VLS_asFunctionalComponent(__VLS_324, new __VLS_324({
        value: "YEAR",
    }));
    var __VLS_326 = __VLS_325.apply(void 0, __spreadArray([{
            value: "YEAR",
        }], __VLS_functionalComponentArgsRest(__VLS_325), false));
    __VLS_327.slots.default;
    var __VLS_327;
    var __VLS_315;
    var __VLS_307;
    var __VLS_303;
    var __VLS_328 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_329 = __VLS_asFunctionalComponent(__VLS_328, new __VLS_328({
        span: (12),
    }));
    var __VLS_330 = __VLS_329.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_329), false));
    __VLS_331.slots.default;
    var __VLS_332 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_333 = __VLS_asFunctionalComponent(__VLS_332, new __VLS_332({
        field: "remindCycle",
        label: "密码到期前提醒周期",
    }));
    var __VLS_334 = __VLS_333.apply(void 0, __spreadArray([{
            field: "remindCycle",
            label: "密码到期前提醒周期",
        }], __VLS_functionalComponentArgsRest(__VLS_333), false));
    __VLS_335.slots.default;
    var __VLS_336 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_337 = __VLS_asFunctionalComponent(__VLS_336, new __VLS_336(__assign({ min: (1), max: (99), step: (1), mode: "button", modelValue: (__VLS_ctx.forceChangePasswordForm.remindCycle) }, { style: {} })));
    var __VLS_338 = __VLS_337.apply(void 0, __spreadArray([__assign({ min: (1), max: (99), step: (1), mode: "button", modelValue: (__VLS_ctx.forceChangePasswordForm.remindCycle) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_337), false));
    var __VLS_340 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_341 = __VLS_asFunctionalComponent(__VLS_340, new __VLS_340(__assign({ modelValue: (__VLS_ctx.forceChangePasswordForm.remindCycleUnit) }, { style: {} })));
    var __VLS_342 = __VLS_341.apply(void 0, __spreadArray([__assign({ modelValue: (__VLS_ctx.forceChangePasswordForm.remindCycleUnit) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_341), false));
    __VLS_343.slots.default;
    var __VLS_344 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_345 = __VLS_asFunctionalComponent(__VLS_344, new __VLS_344({
        value: "DAY",
    }));
    var __VLS_346 = __VLS_345.apply(void 0, __spreadArray([{
            value: "DAY",
        }], __VLS_functionalComponentArgsRest(__VLS_345), false));
    __VLS_347.slots.default;
    var __VLS_347;
    var __VLS_348 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_349 = __VLS_asFunctionalComponent(__VLS_348, new __VLS_348({
        value: "MONTH",
    }));
    var __VLS_350 = __VLS_349.apply(void 0, __spreadArray([{
            value: "MONTH",
        }], __VLS_functionalComponentArgsRest(__VLS_349), false));
    __VLS_351.slots.default;
    var __VLS_351;
    var __VLS_352 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_353 = __VLS_asFunctionalComponent(__VLS_352, new __VLS_352({
        value: "YEAR",
    }));
    var __VLS_354 = __VLS_353.apply(void 0, __spreadArray([{
            value: "YEAR",
        }], __VLS_functionalComponentArgsRest(__VLS_353), false));
    __VLS_355.slots.default;
    var __VLS_355;
    var __VLS_343;
    {
        var __VLS_thisSlot = __VLS_335.slots.extra;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    }
    var __VLS_335;
    var __VLS_331;
    var __VLS_299;
}
var __VLS_285;
var __VLS_356 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_357 = __VLS_asFunctionalComponent(__VLS_356, new __VLS_356({}));
var __VLS_358 = __VLS_357.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_357), false));
__VLS_359.slots.default;
var __VLS_360 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_361 = __VLS_asFunctionalComponent(__VLS_360, new __VLS_360(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_362 = __VLS_361.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_361), false));
var __VLS_364;
var __VLS_365;
var __VLS_366;
var __VLS_367 = {
    onClick: (__VLS_ctx.handleUpdatePasswordPolicyFormSubmit)
};
__VLS_363.slots.default;
var __VLS_363;
var __VLS_368 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_369 = __VLS_asFunctionalComponent(__VLS_368, new __VLS_368(__assign({ 'onClick': {} })));
var __VLS_370 = __VLS_369.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_369), false));
var __VLS_372;
var __VLS_373;
var __VLS_374;
var __VLS_375 = {
    onClick: (__VLS_ctx.handleResetUpdatePasswordPolicyForm)
};
__VLS_371.slots.default;
var __VLS_371;
var __VLS_359;
var __VLS_3;
var __VLS_376 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_377 = __VLS_asFunctionalComponent(__VLS_376, new __VLS_376(__assign(__assign({ 'onClose': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.specificPasswordListEditModalVisible), footer: (false), title: "编辑特定密码", width: "620px" })));
var __VLS_378 = __VLS_377.apply(void 0, __spreadArray([__assign(__assign({ 'onClose': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.specificPasswordListEditModalVisible), footer: (false), title: "编辑特定密码", width: "620px" })], __VLS_functionalComponentArgsRest(__VLS_377), false));
var __VLS_380;
var __VLS_381;
var __VLS_382;
var __VLS_383 = {
    onClose: (function () { return (__VLS_ctx.specificPasswordListEditModalVisible = false); })
};
var __VLS_384 = {
    onCancel: (function () { return (__VLS_ctx.specificPasswordListEditModalVisible = false); })
};
__VLS_379.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "edit-specific-password-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "list-container" }));
if (__VLS_ctx.passwordStrengthForm.specificPasswordList.length === 0) {
    var __VLS_385 = {}.AEmpty;
    /** @type {[typeof __VLS_components.AEmpty, typeof __VLS_components.aEmpty, ]} */ ;
    // @ts-ignore
    var __VLS_386 = __VLS_asFunctionalComponent(__VLS_385, new __VLS_385({}));
    var __VLS_387 = __VLS_386.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_386), false));
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "input-container" }));
    var _loop_1 = function (item, i) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "input-item" }, { key: (i) }));
        var __VLS_389 = {}.AInput;
        /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
        // @ts-ignore
        var __VLS_390 = __VLS_asFunctionalComponent(__VLS_389, new __VLS_389({
            modelValue: (__VLS_ctx.passwordStrengthForm.specificPasswordList[i]),
            placeholder: "请输入密码",
        }));
        var __VLS_391 = __VLS_390.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.passwordStrengthForm.specificPasswordList[i]),
                placeholder: "请输入密码",
            }], __VLS_functionalComponentArgsRest(__VLS_390), false));
        __VLS_392.slots.default;
        {
            var __VLS_thisSlot = __VLS_392.slots.prefix;
            (i + 1);
        }
        var __VLS_393 = {}.IconMinusCircle;
        /** @type {[typeof __VLS_components.IconMinusCircle, typeof __VLS_components.iconMinusCircle, ]} */ ;
        // @ts-ignore
        var __VLS_394 = __VLS_asFunctionalComponent(__VLS_393, new __VLS_393(__assign({ 'onClick': {} }, { style: {} })));
        var __VLS_395 = __VLS_394.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_394), false));
        var __VLS_397 = void 0;
        var __VLS_398 = void 0;
        var __VLS_399 = void 0;
        var __VLS_400 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!!(__VLS_ctx.passwordStrengthForm.specificPasswordList.length === 0))
                    return;
                __VLS_ctx.handleDeleteSpecificPasswordInputItem(i);
            }
        };
    };
    var __VLS_392, __VLS_396;
    for (var _g = 0, _h = __VLS_getVForSourceType((__VLS_ctx.passwordStrengthForm.specificPasswordList)); _g < _h.length; _g++) {
        var _j = _h[_g], item = _j[0], i = _j[1];
        _loop_1(item, i);
    }
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: (__VLS_ctx.handleAddSpecificPasswordInputItem) }, { class: "add-btn-container" }));
var __VLS_401 = {}.IconPlus;
/** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
// @ts-ignore
var __VLS_402 = __VLS_asFunctionalComponent(__VLS_401, new __VLS_401(__assign({ style: {} })));
var __VLS_403 = __VLS_402.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_402), false));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_379;
/** @type {__VLS_StyleScopedClasses['detail-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['password-strength-label']} */ ;
/** @type {__VLS_StyleScopedClasses['password-strength-tag']} */ ;
/** @type {__VLS_StyleScopedClasses['select-char-type-container']} */ ;
/** @type {__VLS_StyleScopedClasses['chat-type-count-select']} */ ;
/** @type {__VLS_StyleScopedClasses['prohibit-password-type-container']} */ ;
/** @type {__VLS_StyleScopedClasses['prohibit-password-type-checkbox']} */ ;
/** @type {__VLS_StyleScopedClasses['prohibit-password-type-checkbox']} */ ;
/** @type {__VLS_StyleScopedClasses['prohibit-password-type-checkbox']} */ ;
/** @type {__VLS_StyleScopedClasses['prohibit-password-type-checkbox']} */ ;
/** @type {__VLS_StyleScopedClasses['prohibit-password-type-checkbox']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['edit-specific-password-container']} */ ;
/** @type {__VLS_StyleScopedClasses['list-container']} */ ;
/** @type {__VLS_StyleScopedClasses['input-container']} */ ;
/** @type {__VLS_StyleScopedClasses['input-item']} */ ;
/** @type {__VLS_StyleScopedClasses['add-btn-container']} */ ;
// @ts-ignore
var __VLS_13 = __VLS_12, __VLS_39 = __VLS_38, __VLS_107 = __VLS_106, __VLS_273 = __VLS_272, __VLS_287 = __VLS_286;
var __VLS_dollars;
var __VLS_self;
