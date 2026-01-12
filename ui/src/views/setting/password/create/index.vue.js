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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "create-title" }));
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
}));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.basicInfoForm.name),
        placeholder: "请输入策略名称",
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
var __VLS_30 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30({
    model: (__VLS_ctx.policyPrincipalForm),
    ref: "policyPrincipalFormRef",
    rules: (__VLS_ctx.policyPrincipalFormRules),
    layout: "vertical",
}));
var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.policyPrincipalForm),
        ref: "policyPrincipalFormRef",
        rules: (__VLS_ctx.policyPrincipalFormRules),
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_31), false));
/** @type {typeof __VLS_ctx.policyPrincipalFormRef} */ ;
var __VLS_34 = {};
__VLS_33.slots.default;
var __VLS_36 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    gutter: (24),
}));
var __VLS_38 = __VLS_37.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_37), false));
__VLS_39.slots.default;
var __VLS_40 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
    span: (12),
}));
var __VLS_42 = __VLS_41.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_41), false));
__VLS_43.slots.default;
var __VLS_44 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    field: "type",
    label: "主体类型",
}));
var __VLS_46 = __VLS_45.apply(void 0, __spreadArray([{
        field: "type",
        label: "主体类型",
    }], __VLS_functionalComponentArgsRest(__VLS_45), false));
__VLS_47.slots.default;
var __VLS_48 = {}.ARadioGroup;
/** @type {[typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, ]} */ ;
// @ts-ignore
var __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48(__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.policyPrincipalForm.type) })));
var __VLS_50 = __VLS_49.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.policyPrincipalForm.type) })], __VLS_functionalComponentArgsRest(__VLS_49), false));
var __VLS_52;
var __VLS_53;
var __VLS_54;
var __VLS_55 = {
    onChange: (__VLS_ctx.principalSelectChange)
};
__VLS_51.slots.default;
var __VLS_56 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
    value: "USER",
}));
var __VLS_58 = __VLS_57.apply(void 0, __spreadArray([{
        value: "USER",
    }], __VLS_functionalComponentArgsRest(__VLS_57), false));
__VLS_59.slots.default;
var __VLS_59;
var __VLS_60 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({
    value: "USER_GROUP",
}));
var __VLS_62 = __VLS_61.apply(void 0, __spreadArray([{
        value: "USER_GROUP",
    }], __VLS_functionalComponentArgsRest(__VLS_61), false));
__VLS_63.slots.default;
var __VLS_63;
var __VLS_51;
var __VLS_47;
var __VLS_43;
var __VLS_64 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
    span: (12),
}));
var __VLS_66 = __VLS_65.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_65), false));
__VLS_67.slots.default;
var __VLS_68 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
    field: "id",
    label: "主体",
}));
var __VLS_70 = __VLS_69.apply(void 0, __spreadArray([{
        field: "id",
        label: "主体",
    }], __VLS_functionalComponentArgsRest(__VLS_69), false));
__VLS_71.slots.default;
if (__VLS_ctx.policyPrincipalForm.type === 'USER') {
    var __VLS_72 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择用户", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.policyPrincipalForm.id), inputValue: (__VLS_ctx.userSearchKeyword), filterOption: (false) })));
    var __VLS_74 = __VLS_73.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择用户", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.policyPrincipalForm.id), inputValue: (__VLS_ctx.userSearchKeyword), filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_73), false));
    var __VLS_76 = void 0;
    var __VLS_77 = void 0;
    var __VLS_78 = void 0;
    var __VLS_79 = {
        onSearch: (__VLS_ctx.handleSearchUser)
    };
    var __VLS_80 = {
        onClear: (__VLS_ctx.handleSearchUser)
    };
    var __VLS_81 = {
        onDropdownReachBottom: (__VLS_ctx.loadMoreUser)
    };
    __VLS_75.slots.default;
    for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.userList)); _i < _a.length; _i++) {
        var user = _a[_i][0];
        var __VLS_82 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
            key: (user.userId),
            value: (user.userId),
        }));
        var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
                key: (user.userId),
                value: (user.userId),
            }], __VLS_functionalComponentArgsRest(__VLS_83), false));
        __VLS_85.slots.default;
        (user.username);
        var __VLS_85;
    }
    var __VLS_75;
}
if (__VLS_ctx.policyPrincipalForm.type === 'USER_GROUP') {
    var __VLS_86 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择用户组", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.policyPrincipalForm.id), inputValue: (__VLS_ctx.userGroupSearchKeyword), filterOption: (false) })));
    var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择用户组", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.policyPrincipalForm.id), inputValue: (__VLS_ctx.userGroupSearchKeyword), filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_87), false));
    var __VLS_90 = void 0;
    var __VLS_91 = void 0;
    var __VLS_92 = void 0;
    var __VLS_93 = {
        onSearch: (__VLS_ctx.handleSearchUserGroup)
    };
    var __VLS_94 = {
        onClear: (__VLS_ctx.handleSearchUserGroup)
    };
    var __VLS_95 = {
        onDropdownReachBottom: (__VLS_ctx.loadMoreUserGroup)
    };
    __VLS_89.slots.default;
    for (var _b = 0, _c = __VLS_getVForSourceType((__VLS_ctx.userGroupList)); _b < _c.length; _b++) {
        var userGroup = _c[_b][0];
        var __VLS_96 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
            key: (userGroup.id),
            value: (userGroup.id),
        }));
        var __VLS_98 = __VLS_97.apply(void 0, __spreadArray([{
                key: (userGroup.id),
                value: (userGroup.id),
            }], __VLS_functionalComponentArgsRest(__VLS_97), false));
        __VLS_99.slots.default;
        (userGroup.name);
        var __VLS_99;
    }
    var __VLS_89;
}
var __VLS_71;
var __VLS_67;
var __VLS_39;
var __VLS_33;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_100 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100({
    model: (__VLS_ctx.passwordStrengthForm),
    ref: "passwordStrengthFormRef",
    rules: (__VLS_ctx.passwordStrengthFormRules),
    layout: "vertical",
}));
var __VLS_102 = __VLS_101.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.passwordStrengthForm),
        ref: "passwordStrengthFormRef",
        rules: (__VLS_ctx.passwordStrengthFormRules),
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_101), false));
/** @type {typeof __VLS_ctx.passwordStrengthFormRef} */ ;
var __VLS_104 = {};
__VLS_103.slots.default;
var __VLS_106 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({
    field: "passwordStrength",
}));
var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{
        field: "passwordStrength",
    }], __VLS_functionalComponentArgsRest(__VLS_107), false));
__VLS_109.slots.default;
{
    var __VLS_thisSlot = __VLS_109.slots.label;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "password-strength-label" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    var __VLS_110 = {}.ATag;
    /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
    // @ts-ignore
    var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110(__assign({ class: "password-strength-tag" }, { color: "gray", size: "small" })));
    var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([__assign({ class: "password-strength-tag" }, { color: "gray", size: "small" })], __VLS_functionalComponentArgsRest(__VLS_111), false));
    __VLS_113.slots.default;
    (__VLS_ctx.passwordStrengthLabel);
    var __VLS_113;
}
var __VLS_114 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114(__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.passwordStrengthForm.passwordStrength), placeholder: "请选择密码规则" })));
var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.passwordStrengthForm.passwordStrength), placeholder: "请选择密码规则" })], __VLS_functionalComponentArgsRest(__VLS_115), false));
var __VLS_118;
var __VLS_119;
var __VLS_120;
var __VLS_121 = {
    onChange: (__VLS_ctx.handleCheckPassword)
};
__VLS_117.slots.default;
var __VLS_122 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
    value: (0),
}));
var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
        value: (0),
    }], __VLS_functionalComponentArgsRest(__VLS_123), false));
__VLS_125.slots.default;
var __VLS_125;
var __VLS_126 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126({
    value: (1),
}));
var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([{
        value: (1),
    }], __VLS_functionalComponentArgsRest(__VLS_127), false));
__VLS_129.slots.default;
var __VLS_129;
var __VLS_130 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130({
    value: (2),
}));
var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{
        value: (2),
    }], __VLS_functionalComponentArgsRest(__VLS_131), false));
__VLS_133.slots.default;
var __VLS_133;
var __VLS_134 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134({
    value: (3),
}));
var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([{
        value: (3),
    }], __VLS_functionalComponentArgsRest(__VLS_135), false));
__VLS_137.slots.default;
var __VLS_137;
var __VLS_138 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_139 = __VLS_asFunctionalComponent(__VLS_138, new __VLS_138({
    value: (4),
}));
var __VLS_140 = __VLS_139.apply(void 0, __spreadArray([{
        value: (4),
    }], __VLS_functionalComponentArgsRest(__VLS_139), false));
__VLS_141.slots.default;
var __VLS_141;
var __VLS_117;
var __VLS_109;
if (__VLS_ctx.passwordStrengthForm.passwordStrength === 4) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_142 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_143 = __VLS_asFunctionalComponent(__VLS_142, new __VLS_142({
        gutter: (24),
    }));
    var __VLS_144 = __VLS_143.apply(void 0, __spreadArray([{
            gutter: (24),
        }], __VLS_functionalComponentArgsRest(__VLS_143), false));
    __VLS_145.slots.default;
    var __VLS_146 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_147 = __VLS_asFunctionalComponent(__VLS_146, new __VLS_146({
        span: (4),
    }));
    var __VLS_148 = __VLS_147.apply(void 0, __spreadArray([{
            span: (4),
        }], __VLS_functionalComponentArgsRest(__VLS_147), false));
    __VLS_149.slots.default;
    var __VLS_150 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_151 = __VLS_asFunctionalComponent(__VLS_150, new __VLS_150({
        field: "minLength",
        label: "密码最小位数",
    }));
    var __VLS_152 = __VLS_151.apply(void 0, __spreadArray([{
            field: "minLength",
            label: "密码最小位数",
        }], __VLS_functionalComponentArgsRest(__VLS_151), false));
    __VLS_153.slots.default;
    var __VLS_154 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_155 = __VLS_asFunctionalComponent(__VLS_154, new __VLS_154({
        step: (1),
        min: (1),
        max: (35),
        mode: "button",
        modelValue: (__VLS_ctx.passwordStrengthForm.minLength),
    }));
    var __VLS_156 = __VLS_155.apply(void 0, __spreadArray([{
            step: (1),
            min: (1),
            max: (35),
            mode: "button",
            modelValue: (__VLS_ctx.passwordStrengthForm.minLength),
        }], __VLS_functionalComponentArgsRest(__VLS_155), false));
    var __VLS_153;
    var __VLS_149;
    var __VLS_158 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_159 = __VLS_asFunctionalComponent(__VLS_158, new __VLS_158({
        span: (4),
    }));
    var __VLS_160 = __VLS_159.apply(void 0, __spreadArray([{
            span: (4),
        }], __VLS_functionalComponentArgsRest(__VLS_159), false));
    __VLS_161.slots.default;
    var __VLS_162 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_163 = __VLS_asFunctionalComponent(__VLS_162, new __VLS_162({
        field: "maxLength",
        label: "密码最大位数",
    }));
    var __VLS_164 = __VLS_163.apply(void 0, __spreadArray([{
            field: "maxLength",
            label: "密码最大位数",
        }], __VLS_functionalComponentArgsRest(__VLS_163), false));
    __VLS_165.slots.default;
    var __VLS_166 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_167 = __VLS_asFunctionalComponent(__VLS_166, new __VLS_166({
        step: (1),
        min: (1),
        max: (35),
        mode: "button",
        modelValue: (__VLS_ctx.passwordStrengthForm.maxLength),
    }));
    var __VLS_168 = __VLS_167.apply(void 0, __spreadArray([{
            step: (1),
            min: (1),
            max: (35),
            mode: "button",
            modelValue: (__VLS_ctx.passwordStrengthForm.maxLength),
        }], __VLS_functionalComponentArgsRest(__VLS_167), false));
    var __VLS_165;
    var __VLS_161;
    var __VLS_170 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_171 = __VLS_asFunctionalComponent(__VLS_170, new __VLS_170({
        span: (16),
    }));
    var __VLS_172 = __VLS_171.apply(void 0, __spreadArray([{
            span: (16),
        }], __VLS_functionalComponentArgsRest(__VLS_171), false));
    __VLS_173.slots.default;
    var __VLS_174 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_175 = __VLS_asFunctionalComponent(__VLS_174, new __VLS_174({
        field: "charType",
        label: "密码字符类型要求",
    }));
    var __VLS_176 = __VLS_175.apply(void 0, __spreadArray([{
            field: "charType",
            label: "密码字符类型要求",
        }], __VLS_functionalComponentArgsRest(__VLS_175), false));
    __VLS_177.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-char-type-container" }));
    var __VLS_178 = {}.ACheckboxGroup;
    /** @type {[typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, ]} */ ;
    // @ts-ignore
    var __VLS_179 = __VLS_asFunctionalComponent(__VLS_178, new __VLS_178(__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.passwordStrengthForm.charType) })));
    var __VLS_180 = __VLS_179.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.passwordStrengthForm.charType) })], __VLS_functionalComponentArgsRest(__VLS_179), false));
    var __VLS_182 = void 0;
    var __VLS_183 = void 0;
    var __VLS_184 = void 0;
    var __VLS_185 = {
        onChange: (function () { return (__VLS_ctx.passwordStrengthForm.minCharTypeCount = 0); })
    };
    __VLS_181.slots.default;
    var __VLS_186 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_187 = __VLS_asFunctionalComponent(__VLS_186, new __VLS_186({
        value: "UPPER_CASE",
    }));
    var __VLS_188 = __VLS_187.apply(void 0, __spreadArray([{
            value: "UPPER_CASE",
        }], __VLS_functionalComponentArgsRest(__VLS_187), false));
    __VLS_189.slots.default;
    var __VLS_189;
    var __VLS_190 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_191 = __VLS_asFunctionalComponent(__VLS_190, new __VLS_190({
        value: "LOWER_CASE",
    }));
    var __VLS_192 = __VLS_191.apply(void 0, __spreadArray([{
            value: "LOWER_CASE",
        }], __VLS_functionalComponentArgsRest(__VLS_191), false));
    __VLS_193.slots.default;
    var __VLS_193;
    var __VLS_194 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_195 = __VLS_asFunctionalComponent(__VLS_194, new __VLS_194({
        value: "NUMBER",
    }));
    var __VLS_196 = __VLS_195.apply(void 0, __spreadArray([{
            value: "NUMBER",
        }], __VLS_functionalComponentArgsRest(__VLS_195), false));
    __VLS_197.slots.default;
    var __VLS_197;
    var __VLS_198 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_199 = __VLS_asFunctionalComponent(__VLS_198, new __VLS_198({
        value: "SPECIAL_CHAR",
    }));
    var __VLS_200 = __VLS_199.apply(void 0, __spreadArray([{
            value: "SPECIAL_CHAR",
        }], __VLS_functionalComponentArgsRest(__VLS_199), false));
    __VLS_201.slots.default;
    var __VLS_201;
    var __VLS_181;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "chat-type-count-select" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_202 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_203 = __VLS_asFunctionalComponent(__VLS_202, new __VLS_202(__assign(__assign({ modelValue: (__VLS_ctx.passwordStrengthForm.minCharTypeCount) }, { style: {} }), { disabled: (__VLS_ctx.passwordStrengthForm.charType.length === 0) })));
    var __VLS_204 = __VLS_203.apply(void 0, __spreadArray([__assign(__assign({ modelValue: (__VLS_ctx.passwordStrengthForm.minCharTypeCount) }, { style: {} }), { disabled: (__VLS_ctx.passwordStrengthForm.charType.length === 0) })], __VLS_functionalComponentArgsRest(__VLS_203), false));
    __VLS_205.slots.default;
    if (__VLS_ctx.passwordStrengthForm.charType.length === 0 ||
        __VLS_ctx.passwordStrengthForm.charType.length === 1) {
        var __VLS_206 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_207 = __VLS_asFunctionalComponent(__VLS_206, new __VLS_206({
            value: (0),
        }));
        var __VLS_208 = __VLS_207.apply(void 0, __spreadArray([{
                value: (0),
            }], __VLS_functionalComponentArgsRest(__VLS_207), false));
        __VLS_209.slots.default;
        var __VLS_209;
    }
    if (__VLS_ctx.passwordStrengthForm.charType.length > 1) {
        for (var _d = 0, _e = __VLS_getVForSourceType((__VLS_ctx.passwordStrengthForm.charType)); _d < _e.length; _d++) {
            var _f = _e[_d], item = _f[0], i = _f[1];
            var __VLS_210 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_211 = __VLS_asFunctionalComponent(__VLS_210, new __VLS_210({
                key: (i),
                value: (i),
            }));
            var __VLS_212 = __VLS_211.apply(void 0, __spreadArray([{
                    key: (i),
                    value: (i),
                }], __VLS_functionalComponentArgsRest(__VLS_211), false));
            __VLS_213.slots.default;
            (i === 0 ? "全部" : "".concat(i, " \u79CD"));
            var __VLS_213;
        }
    }
    var __VLS_205;
    var __VLS_177;
    var __VLS_173;
    var __VLS_145;
    var __VLS_214 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_215 = __VLS_asFunctionalComponent(__VLS_214, new __VLS_214({
        label: "禁止使用的密码类型配置",
    }));
    var __VLS_216 = __VLS_215.apply(void 0, __spreadArray([{
            label: "禁止使用的密码类型配置",
        }], __VLS_functionalComponentArgsRest(__VLS_215), false));
    __VLS_217.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "prohibit-password-type-container" }));
    var __VLS_218 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_219 = __VLS_asFunctionalComponent(__VLS_218, new __VLS_218(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitUserInfo) })));
    var __VLS_220 = __VLS_219.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitUserInfo) })], __VLS_functionalComponentArgsRest(__VLS_219), false));
    __VLS_221.slots.default;
    var __VLS_221;
    var __VLS_222 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_223 = __VLS_asFunctionalComponent(__VLS_222, new __VLS_222(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitSingleChar) })));
    var __VLS_224 = __VLS_223.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitSingleChar) })], __VLS_functionalComponentArgsRest(__VLS_223), false));
    __VLS_225.slots.default;
    var __VLS_225;
    var __VLS_226 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_227 = __VLS_asFunctionalComponent(__VLS_226, new __VLS_226(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitConsecutiveChar) })));
    var __VLS_228 = __VLS_227.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitConsecutiveChar) })], __VLS_functionalComponentArgsRest(__VLS_227), false));
    __VLS_229.slots.default;
    var __VLS_229;
    var __VLS_230 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_231 = __VLS_asFunctionalComponent(__VLS_230, new __VLS_230(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitContainConsecutiveChar) })));
    var __VLS_232 = __VLS_231.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitContainConsecutiveChar) })], __VLS_functionalComponentArgsRest(__VLS_231), false));
    __VLS_233.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_234 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_235 = __VLS_asFunctionalComponent(__VLS_234, new __VLS_234(__assign({ style: {} }, { min: (2), max: (35), step: (1), mode: "button", size: "mini", modelValue: (__VLS_ctx.passwordStrengthForm.minConsecutiveCharLength), disabled: (!__VLS_ctx.passwordStrengthForm.prohibitContainConsecutiveChar) })));
    var __VLS_236 = __VLS_235.apply(void 0, __spreadArray([__assign({ style: {} }, { min: (2), max: (35), step: (1), mode: "button", size: "mini", modelValue: (__VLS_ctx.passwordStrengthForm.minConsecutiveCharLength), disabled: (!__VLS_ctx.passwordStrengthForm.prohibitContainConsecutiveChar) })], __VLS_functionalComponentArgsRest(__VLS_235), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_233;
    var __VLS_238 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_239 = __VLS_asFunctionalComponent(__VLS_238, new __VLS_238(__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitContainRepeatChar) })));
    var __VLS_240 = __VLS_239.apply(void 0, __spreadArray([__assign({ class: "prohibit-password-type-checkbox" }, { modelValue: (__VLS_ctx.passwordStrengthForm.prohibitContainRepeatChar) })], __VLS_functionalComponentArgsRest(__VLS_239), false));
    __VLS_241.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_242 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_243 = __VLS_asFunctionalComponent(__VLS_242, new __VLS_242(__assign({ style: {} }, { min: (2), max: (35), step: (1), mode: "button", size: "mini", modelValue: (__VLS_ctx.passwordStrengthForm.minRepeatCharLength), disabled: (!__VLS_ctx.passwordStrengthForm.prohibitContainRepeatChar) })));
    var __VLS_244 = __VLS_243.apply(void 0, __spreadArray([__assign({ style: {} }, { min: (2), max: (35), step: (1), mode: "button", size: "mini", modelValue: (__VLS_ctx.passwordStrengthForm.minRepeatCharLength), disabled: (!__VLS_ctx.passwordStrengthForm.prohibitContainRepeatChar) })], __VLS_functionalComponentArgsRest(__VLS_243), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_241;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
    var __VLS_246 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_247 = __VLS_asFunctionalComponent(__VLS_246, new __VLS_246({
        modelValue: (__VLS_ctx.passwordStrengthForm.prohibitSpecificPassword),
    }));
    var __VLS_248 = __VLS_247.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.passwordStrengthForm.prohibitSpecificPassword),
        }], __VLS_functionalComponentArgsRest(__VLS_247), false));
    __VLS_249.slots.default;
    var __VLS_249;
    var __VLS_250 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_251 = __VLS_asFunctionalComponent(__VLS_250, new __VLS_250(__assign(__assign({ 'onClick': {} }, { type: "text", size: "mini" }), { style: {} })));
    var __VLS_252 = __VLS_251.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { type: "text", size: "mini" }), { style: {} })], __VLS_functionalComponentArgsRest(__VLS_251), false));
    var __VLS_254 = void 0;
    var __VLS_255 = void 0;
    var __VLS_256 = void 0;
    var __VLS_257 = {
        onClick: (function () { return (__VLS_ctx.specificPasswordListEditModalVisible = true); })
    };
    __VLS_253.slots.default;
    var __VLS_253;
    var __VLS_217;
}
var __VLS_258 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_259 = __VLS_asFunctionalComponent(__VLS_258, new __VLS_258({
    label: "检查密码",
}));
var __VLS_260 = __VLS_259.apply(void 0, __spreadArray([{
        label: "检查密码",
    }], __VLS_functionalComponentArgsRest(__VLS_259), false));
__VLS_261.slots.default;
var __VLS_262 = {}.PasswordChecker;
/** @type {[typeof __VLS_components.PasswordChecker, typeof __VLS_components.passwordChecker, ]} */ ;
// @ts-ignore
var __VLS_263 = __VLS_asFunctionalComponent(__VLS_262, new __VLS_262(__assign({ 'onCheck': {} }, { ref: "passwordCheckerRef", type: "text", loading: (__VLS_ctx.checkLoading), checkRes: (__VLS_ctx.checkRes) })));
var __VLS_264 = __VLS_263.apply(void 0, __spreadArray([__assign({ 'onCheck': {} }, { ref: "passwordCheckerRef", type: "text", loading: (__VLS_ctx.checkLoading), checkRes: (__VLS_ctx.checkRes) })], __VLS_functionalComponentArgsRest(__VLS_263), false));
var __VLS_266;
var __VLS_267;
var __VLS_268;
var __VLS_269 = {
    onCheck: (__VLS_ctx.handleCheckPassword)
};
/** @type {typeof __VLS_ctx.passwordCheckerRef} */ ;
var __VLS_270 = {};
var __VLS_265;
var __VLS_261;
var __VLS_272 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_273 = __VLS_asFunctionalComponent(__VLS_272, new __VLS_272({
    label: "用户登录密码强度检查",
}));
var __VLS_274 = __VLS_273.apply(void 0, __spreadArray([{
        label: "用户登录密码强度检查",
    }], __VLS_functionalComponentArgsRest(__VLS_273), false));
__VLS_275.slots.default;
var __VLS_276 = {}.ASwitch;
/** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
// @ts-ignore
var __VLS_277 = __VLS_asFunctionalComponent(__VLS_276, new __VLS_276({
    type: "round",
    modelValue: (__VLS_ctx.passwordStrengthForm.enablePasswordDetection),
}));
var __VLS_278 = __VLS_277.apply(void 0, __spreadArray([{
        type: "round",
        modelValue: (__VLS_ctx.passwordStrengthForm.enablePasswordDetection),
    }], __VLS_functionalComponentArgsRest(__VLS_277), false));
{
    var __VLS_thisSlot = __VLS_275.slots.extra;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
}
var __VLS_275;
var __VLS_103;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_280 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_281 = __VLS_asFunctionalComponent(__VLS_280, new __VLS_280({
    model: (__VLS_ctx.forceChangePasswordForm),
    ref: "forceChangePasswordFormRef",
    rules: (__VLS_ctx.forceChangePasswordFormRules),
    layout: "vertical",
}));
var __VLS_282 = __VLS_281.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.forceChangePasswordForm),
        ref: "forceChangePasswordFormRef",
        rules: (__VLS_ctx.forceChangePasswordFormRules),
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_281), false));
/** @type {typeof __VLS_ctx.forceChangePasswordFormRef} */ ;
var __VLS_284 = {};
__VLS_283.slots.default;
var __VLS_286 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_287 = __VLS_asFunctionalComponent(__VLS_286, new __VLS_286({
    label: "强制用户定期修改密码",
}));
var __VLS_288 = __VLS_287.apply(void 0, __spreadArray([{
        label: "强制用户定期修改密码",
    }], __VLS_functionalComponentArgsRest(__VLS_287), false));
__VLS_289.slots.default;
var __VLS_290 = {}.ASwitch;
/** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
// @ts-ignore
var __VLS_291 = __VLS_asFunctionalComponent(__VLS_290, new __VLS_290({
    type: "round",
    modelValue: (__VLS_ctx.forceChangePasswordForm.enableForceChangePassword),
}));
var __VLS_292 = __VLS_291.apply(void 0, __spreadArray([{
        type: "round",
        modelValue: (__VLS_ctx.forceChangePasswordForm.enableForceChangePassword),
    }], __VLS_functionalComponentArgsRest(__VLS_291), false));
{
    var __VLS_thisSlot = __VLS_289.slots.extra;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
}
var __VLS_289;
if (__VLS_ctx.forceChangePasswordForm.enableForceChangePassword) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_294 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_295 = __VLS_asFunctionalComponent(__VLS_294, new __VLS_294({
        gutter: (24),
    }));
    var __VLS_296 = __VLS_295.apply(void 0, __spreadArray([{
            gutter: (24),
        }], __VLS_functionalComponentArgsRest(__VLS_295), false));
    __VLS_297.slots.default;
    var __VLS_298 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_299 = __VLS_asFunctionalComponent(__VLS_298, new __VLS_298({
        span: (12),
    }));
    var __VLS_300 = __VLS_299.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_299), false));
    __VLS_301.slots.default;
    var __VLS_302 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_303 = __VLS_asFunctionalComponent(__VLS_302, new __VLS_302({
        field: "forcedCycle",
        label: "强制修改密码周期",
    }));
    var __VLS_304 = __VLS_303.apply(void 0, __spreadArray([{
            field: "forcedCycle",
            label: "强制修改密码周期",
        }], __VLS_functionalComponentArgsRest(__VLS_303), false));
    __VLS_305.slots.default;
    var __VLS_306 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_307 = __VLS_asFunctionalComponent(__VLS_306, new __VLS_306(__assign({ min: (1), max: (99), step: (1), mode: "button", modelValue: (__VLS_ctx.forceChangePasswordForm.forcedCycle) }, { style: {} })));
    var __VLS_308 = __VLS_307.apply(void 0, __spreadArray([__assign({ min: (1), max: (99), step: (1), mode: "button", modelValue: (__VLS_ctx.forceChangePasswordForm.forcedCycle) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_307), false));
    var __VLS_310 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_311 = __VLS_asFunctionalComponent(__VLS_310, new __VLS_310(__assign({ modelValue: (__VLS_ctx.forceChangePasswordForm.forcedCycleUnit) }, { style: {} })));
    var __VLS_312 = __VLS_311.apply(void 0, __spreadArray([__assign({ modelValue: (__VLS_ctx.forceChangePasswordForm.forcedCycleUnit) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_311), false));
    __VLS_313.slots.default;
    var __VLS_314 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_315 = __VLS_asFunctionalComponent(__VLS_314, new __VLS_314({
        value: "DAY",
    }));
    var __VLS_316 = __VLS_315.apply(void 0, __spreadArray([{
            value: "DAY",
        }], __VLS_functionalComponentArgsRest(__VLS_315), false));
    __VLS_317.slots.default;
    var __VLS_317;
    var __VLS_318 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_319 = __VLS_asFunctionalComponent(__VLS_318, new __VLS_318({
        value: "MONTH",
    }));
    var __VLS_320 = __VLS_319.apply(void 0, __spreadArray([{
            value: "MONTH",
        }], __VLS_functionalComponentArgsRest(__VLS_319), false));
    __VLS_321.slots.default;
    var __VLS_321;
    var __VLS_322 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_323 = __VLS_asFunctionalComponent(__VLS_322, new __VLS_322({
        value: "YEAR",
    }));
    var __VLS_324 = __VLS_323.apply(void 0, __spreadArray([{
            value: "YEAR",
        }], __VLS_functionalComponentArgsRest(__VLS_323), false));
    __VLS_325.slots.default;
    var __VLS_325;
    var __VLS_313;
    var __VLS_305;
    var __VLS_301;
    var __VLS_326 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_327 = __VLS_asFunctionalComponent(__VLS_326, new __VLS_326({
        span: (12),
    }));
    var __VLS_328 = __VLS_327.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_327), false));
    __VLS_329.slots.default;
    var __VLS_330 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_331 = __VLS_asFunctionalComponent(__VLS_330, new __VLS_330({
        field: "remindCycle",
        label: "密码到期前提醒周期",
    }));
    var __VLS_332 = __VLS_331.apply(void 0, __spreadArray([{
            field: "remindCycle",
            label: "密码到期前提醒周期",
        }], __VLS_functionalComponentArgsRest(__VLS_331), false));
    __VLS_333.slots.default;
    var __VLS_334 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_335 = __VLS_asFunctionalComponent(__VLS_334, new __VLS_334(__assign({ min: (1), max: (99), step: (1), mode: "button", modelValue: (__VLS_ctx.forceChangePasswordForm.remindCycle) }, { style: {} })));
    var __VLS_336 = __VLS_335.apply(void 0, __spreadArray([__assign({ min: (1), max: (99), step: (1), mode: "button", modelValue: (__VLS_ctx.forceChangePasswordForm.remindCycle) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_335), false));
    var __VLS_338 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_339 = __VLS_asFunctionalComponent(__VLS_338, new __VLS_338(__assign({ modelValue: (__VLS_ctx.forceChangePasswordForm.remindCycleUnit) }, { style: {} })));
    var __VLS_340 = __VLS_339.apply(void 0, __spreadArray([__assign({ modelValue: (__VLS_ctx.forceChangePasswordForm.remindCycleUnit) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_339), false));
    __VLS_341.slots.default;
    var __VLS_342 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_343 = __VLS_asFunctionalComponent(__VLS_342, new __VLS_342({
        value: "DAY",
    }));
    var __VLS_344 = __VLS_343.apply(void 0, __spreadArray([{
            value: "DAY",
        }], __VLS_functionalComponentArgsRest(__VLS_343), false));
    __VLS_345.slots.default;
    var __VLS_345;
    var __VLS_346 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_347 = __VLS_asFunctionalComponent(__VLS_346, new __VLS_346({
        value: "MONTH",
    }));
    var __VLS_348 = __VLS_347.apply(void 0, __spreadArray([{
            value: "MONTH",
        }], __VLS_functionalComponentArgsRest(__VLS_347), false));
    __VLS_349.slots.default;
    var __VLS_349;
    var __VLS_350 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_351 = __VLS_asFunctionalComponent(__VLS_350, new __VLS_350({
        value: "YEAR",
    }));
    var __VLS_352 = __VLS_351.apply(void 0, __spreadArray([{
            value: "YEAR",
        }], __VLS_functionalComponentArgsRest(__VLS_351), false));
    __VLS_353.slots.default;
    var __VLS_353;
    var __VLS_341;
    {
        var __VLS_thisSlot = __VLS_333.slots.extra;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    }
    var __VLS_333;
    var __VLS_329;
    var __VLS_297;
}
var __VLS_283;
var __VLS_354 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_355 = __VLS_asFunctionalComponent(__VLS_354, new __VLS_354({}));
var __VLS_356 = __VLS_355.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_355), false));
__VLS_357.slots.default;
var __VLS_358 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_359 = __VLS_asFunctionalComponent(__VLS_358, new __VLS_358(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_360 = __VLS_359.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_359), false));
var __VLS_362;
var __VLS_363;
var __VLS_364;
var __VLS_365 = {
    onClick: (__VLS_ctx.handleCreatePasswordPolicyFormSubmit)
};
__VLS_361.slots.default;
var __VLS_361;
var __VLS_366 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_367 = __VLS_asFunctionalComponent(__VLS_366, new __VLS_366(__assign({ 'onClick': {} })));
var __VLS_368 = __VLS_367.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_367), false));
var __VLS_370;
var __VLS_371;
var __VLS_372;
var __VLS_373 = {
    onClick: (__VLS_ctx.handleResetCreatePasswordPolicyForm)
};
__VLS_369.slots.default;
var __VLS_369;
var __VLS_357;
var __VLS_3;
var __VLS_374 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_375 = __VLS_asFunctionalComponent(__VLS_374, new __VLS_374(__assign(__assign({ 'onClose': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.specificPasswordListEditModalVisible), footer: (false), title: "编辑特定密码", width: "620px" })));
var __VLS_376 = __VLS_375.apply(void 0, __spreadArray([__assign(__assign({ 'onClose': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.specificPasswordListEditModalVisible), footer: (false), title: "编辑特定密码", width: "620px" })], __VLS_functionalComponentArgsRest(__VLS_375), false));
var __VLS_378;
var __VLS_379;
var __VLS_380;
var __VLS_381 = {
    onClose: (function () { return (__VLS_ctx.specificPasswordListEditModalVisible = false); })
};
var __VLS_382 = {
    onCancel: (function () { return (__VLS_ctx.specificPasswordListEditModalVisible = false); })
};
__VLS_377.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "edit-specific-password-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "list-container" }));
if (__VLS_ctx.passwordStrengthForm.specificPasswordList.length === 0) {
    var __VLS_383 = {}.AEmpty;
    /** @type {[typeof __VLS_components.AEmpty, typeof __VLS_components.aEmpty, ]} */ ;
    // @ts-ignore
    var __VLS_384 = __VLS_asFunctionalComponent(__VLS_383, new __VLS_383({}));
    var __VLS_385 = __VLS_384.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_384), false));
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "input-container" }));
    var _loop_1 = function (item, i) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "input-item" }, { key: (i) }));
        var __VLS_387 = {}.AInput;
        /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
        // @ts-ignore
        var __VLS_388 = __VLS_asFunctionalComponent(__VLS_387, new __VLS_387({
            modelValue: (__VLS_ctx.passwordStrengthForm.specificPasswordList[i]),
            placeholder: "请输入密码",
        }));
        var __VLS_389 = __VLS_388.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.passwordStrengthForm.specificPasswordList[i]),
                placeholder: "请输入密码",
            }], __VLS_functionalComponentArgsRest(__VLS_388), false));
        __VLS_390.slots.default;
        {
            var __VLS_thisSlot = __VLS_390.slots.prefix;
            (i + 1);
        }
        var __VLS_391 = {}.IconMinusCircle;
        /** @type {[typeof __VLS_components.IconMinusCircle, typeof __VLS_components.iconMinusCircle, ]} */ ;
        // @ts-ignore
        var __VLS_392 = __VLS_asFunctionalComponent(__VLS_391, new __VLS_391(__assign({ 'onClick': {} }, { style: {} })));
        var __VLS_393 = __VLS_392.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_392), false));
        var __VLS_395 = void 0;
        var __VLS_396 = void 0;
        var __VLS_397 = void 0;
        var __VLS_398 = {
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
    var __VLS_390, __VLS_394;
    for (var _g = 0, _h = __VLS_getVForSourceType((__VLS_ctx.passwordStrengthForm.specificPasswordList)); _g < _h.length; _g++) {
        var _j = _h[_g], item = _j[0], i = _j[1];
        _loop_1(item, i);
    }
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: (__VLS_ctx.handleAddSpecificPasswordInputItem) }, { class: "add-btn-container" }));
var __VLS_399 = {}.IconPlus;
/** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
// @ts-ignore
var __VLS_400 = __VLS_asFunctionalComponent(__VLS_399, new __VLS_399(__assign({ style: {} })));
var __VLS_401 = __VLS_400.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_400), false));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_377;
/** @type {__VLS_StyleScopedClasses['create-title']} */ ;
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
var __VLS_13 = __VLS_12, __VLS_35 = __VLS_34, __VLS_105 = __VLS_104, __VLS_271 = __VLS_270, __VLS_285 = __VLS_284;
var __VLS_dollars;
var __VLS_self;
