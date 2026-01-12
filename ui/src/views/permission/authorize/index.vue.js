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
import authorizeTs from "./index";
export default authorizeTs;
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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "resource-group-container" }));
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
var __VLS_16 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({
    modelValue: (__VLS_ctx.resourceGroup.name),
    disabled: true,
}));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.resourceGroup.name),
        disabled: true,
    }], __VLS_functionalComponentArgsRest(__VLS_17), false));
var __VLS_15;
var __VLS_11;
if (__VLS_ctx.princialSelectable) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
}
if (__VLS_ctx.princialSelectable) {
    var __VLS_20 = {}.AForm;
    /** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
    // @ts-ignore
    var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({
        model: (__VLS_ctx.principalForm),
        rules: (__VLS_ctx.principalFormRules),
        layout: "vertical",
        ref: "principalFormRef",
    }));
    var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{
            model: (__VLS_ctx.principalForm),
            rules: (__VLS_ctx.principalFormRules),
            layout: "vertical",
            ref: "principalFormRef",
        }], __VLS_functionalComponentArgsRest(__VLS_21), false));
    /** @type {typeof __VLS_ctx.principalFormRef} */ ;
    var __VLS_24 = {};
    __VLS_23.slots.default;
    var __VLS_26 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26({
        gutter: (24),
    }));
    var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([{
            gutter: (24),
        }], __VLS_functionalComponentArgsRest(__VLS_27), false));
    __VLS_29.slots.default;
    var __VLS_30 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30({
        span: (12),
    }));
    var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_31), false));
    __VLS_33.slots.default;
    var __VLS_34 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
        field: "type",
        label: "主体类型",
    }));
    var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
            field: "type",
            label: "主体类型",
        }], __VLS_functionalComponentArgsRest(__VLS_35), false));
    __VLS_37.slots.default;
    var __VLS_38 = {}.ARadioGroup;
    /** @type {[typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, ]} */ ;
    // @ts-ignore
    var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38(__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.principalForm.type) })));
    var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.principalForm.type) })], __VLS_functionalComponentArgsRest(__VLS_39), false));
    var __VLS_42 = void 0;
    var __VLS_43 = void 0;
    var __VLS_44 = void 0;
    var __VLS_45 = {
        onChange: (__VLS_ctx.principalSelectChange)
    };
    __VLS_41.slots.default;
    var __VLS_46 = {}.ARadio;
    /** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
    // @ts-ignore
    var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
        value: "USER",
    }));
    var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
            value: "USER",
        }], __VLS_functionalComponentArgsRest(__VLS_47), false));
    __VLS_49.slots.default;
    var __VLS_49;
    var __VLS_50 = {}.ARadio;
    /** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
    // @ts-ignore
    var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
        value: "USER_GROUP",
    }));
    var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
            value: "USER_GROUP",
        }], __VLS_functionalComponentArgsRest(__VLS_51), false));
    __VLS_53.slots.default;
    var __VLS_53;
    var __VLS_54 = {}.ARadio;
    /** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
    // @ts-ignore
    var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
        value: "ROLE",
    }));
    var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
            value: "ROLE",
        }], __VLS_functionalComponentArgsRest(__VLS_55), false));
    __VLS_57.slots.default;
    var __VLS_57;
    var __VLS_41;
    var __VLS_37;
    var __VLS_33;
    var __VLS_29;
    var __VLS_58 = {}.ARow;
    /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
    // @ts-ignore
    var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
        gutter: (24),
    }));
    var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
            gutter: (24),
        }], __VLS_functionalComponentArgsRest(__VLS_59), false));
    __VLS_61.slots.default;
    var __VLS_62 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
        span: (12),
    }));
    var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_63), false));
    __VLS_65.slots.default;
    var __VLS_66 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
        field: "id",
        label: "主体",
    }));
    var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
            field: "id",
            label: "主体",
        }], __VLS_functionalComponentArgsRest(__VLS_67), false));
    __VLS_69.slots.default;
    if (__VLS_ctx.principalForm.type === 'USER') {
        var __VLS_70 = {}.ASelect;
        /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
        // @ts-ignore
        var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择用户", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.principalForm.id), inputValue: (__VLS_ctx.userSearchKeyword), filterOption: (false) })));
        var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择用户", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.principalForm.id), inputValue: (__VLS_ctx.userSearchKeyword), filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_71), false));
        var __VLS_74 = void 0;
        var __VLS_75 = void 0;
        var __VLS_76 = void 0;
        var __VLS_77 = {
            onSearch: (__VLS_ctx.handleSearchUser)
        };
        var __VLS_78 = {
            onClear: (__VLS_ctx.handleSearchUser)
        };
        var __VLS_79 = {
            onDropdownReachBottom: (__VLS_ctx.loadMoreUser)
        };
        __VLS_73.slots.default;
        for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.userList)); _i < _a.length; _i++) {
            var user = _a[_i][0];
            var __VLS_80 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({
                key: (user.userId),
                value: (user.userId),
            }));
            var __VLS_82 = __VLS_81.apply(void 0, __spreadArray([{
                    key: (user.userId),
                    value: (user.userId),
                }], __VLS_functionalComponentArgsRest(__VLS_81), false));
            __VLS_83.slots.default;
            (user.username);
            var __VLS_83;
        }
        var __VLS_73;
    }
    if (__VLS_ctx.principalForm.type === 'USER_GROUP') {
        var __VLS_84 = {}.ASelect;
        /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
        // @ts-ignore
        var __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择用户组", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.principalForm.id), inputValue: (__VLS_ctx.userGroupSearchKeyword), filterOption: (false) })));
        var __VLS_86 = __VLS_85.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择用户组", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.principalForm.id), inputValue: (__VLS_ctx.userGroupSearchKeyword), filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_85), false));
        var __VLS_88 = void 0;
        var __VLS_89 = void 0;
        var __VLS_90 = void 0;
        var __VLS_91 = {
            onSearch: (__VLS_ctx.handleSearchUserGroup)
        };
        var __VLS_92 = {
            onClear: (__VLS_ctx.handleSearchUserGroup)
        };
        var __VLS_93 = {
            onDropdownReachBottom: (__VLS_ctx.loadMoreUserGroup)
        };
        __VLS_87.slots.default;
        for (var _b = 0, _c = __VLS_getVForSourceType((__VLS_ctx.userGroupList)); _b < _c.length; _b++) {
            var userGroup = _c[_b][0];
            var __VLS_94 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
                key: (userGroup.id),
                value: (userGroup.id),
            }));
            var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
                    key: (userGroup.id),
                    value: (userGroup.id),
                }], __VLS_functionalComponentArgsRest(__VLS_95), false));
            __VLS_97.slots.default;
            (userGroup.name);
            var __VLS_97;
        }
        var __VLS_87;
    }
    if (__VLS_ctx.principalForm.type === 'ROLE') {
        var __VLS_98 = {}.ASelect;
        /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
        // @ts-ignore
        var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择角色", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.principalForm.id), inputValue: (__VLS_ctx.roleSearchKeyword), filterOption: (false) })));
        var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择角色", allowClear: true, allowSearch: true, multiple: true, modelValue: (__VLS_ctx.principalForm.id), inputValue: (__VLS_ctx.roleSearchKeyword), filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_99), false));
        var __VLS_102 = void 0;
        var __VLS_103 = void 0;
        var __VLS_104 = void 0;
        var __VLS_105 = {
            onSearch: (__VLS_ctx.handleSearchRole)
        };
        var __VLS_106 = {
            onClear: (__VLS_ctx.handleSearchRole)
        };
        var __VLS_107 = {
            onDropdownReachBottom: (__VLS_ctx.loadMoreRole)
        };
        __VLS_101.slots.default;
        for (var _d = 0, _e = __VLS_getVForSourceType((__VLS_ctx.roleList)); _d < _e.length; _d++) {
            var role = _e[_d][0];
            var __VLS_108 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
                key: (role.id),
                value: (role.id),
            }));
            var __VLS_110 = __VLS_109.apply(void 0, __spreadArray([{
                    key: (role.id),
                    value: (role.id),
                }], __VLS_functionalComponentArgsRest(__VLS_109), false));
            __VLS_111.slots.default;
            (role.name);
            var __VLS_111;
        }
        var __VLS_101;
    }
    var __VLS_69;
    var __VLS_65;
    var __VLS_61;
    var __VLS_23;
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_112 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112(__assign({ 'onSubmit': {} }, { model: (__VLS_ctx.authorizeForm), rules: (__VLS_ctx.authorizeFormRules), layout: "vertical", ref: "authorizeFormRef" })));
var __VLS_114 = __VLS_113.apply(void 0, __spreadArray([__assign({ 'onSubmit': {} }, { model: (__VLS_ctx.authorizeForm), rules: (__VLS_ctx.authorizeFormRules), layout: "vertical", ref: "authorizeFormRef" })], __VLS_functionalComponentArgsRest(__VLS_113), false));
var __VLS_116;
var __VLS_117;
var __VLS_118;
var __VLS_119 = {
    onSubmit: (__VLS_ctx.handleAuthorizeFormSubmit)
};
/** @type {typeof __VLS_ctx.authorizeFormRef} */ ;
var __VLS_120 = {};
__VLS_115.slots.default;
var __VLS_122 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
    gutter: (24),
}));
var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_123), false));
__VLS_125.slots.default;
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
    field: "resourceId",
    label: "资源",
}));
var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{
        field: "resourceId",
        label: "资源",
    }], __VLS_functionalComponentArgsRest(__VLS_131), false));
__VLS_133.slots.default;
var __VLS_134 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134(__assign(__assign(__assign(__assign({ 'onChange': {} }, { 'onSearch': {} }), { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择资源", allowClear: true, allowSearch: true, modelValue: (__VLS_ctx.authorizeForm.resourceId), inputValue: (__VLS_ctx.resourceSearchKeyword), filterOption: (false) })));
var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onChange': {} }, { 'onSearch': {} }), { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择资源", allowClear: true, allowSearch: true, modelValue: (__VLS_ctx.authorizeForm.resourceId), inputValue: (__VLS_ctx.resourceSearchKeyword), filterOption: (false) })], __VLS_functionalComponentArgsRest(__VLS_135), false));
var __VLS_138;
var __VLS_139;
var __VLS_140;
var __VLS_141 = {
    onChange: (__VLS_ctx.handleSearchPermission)
};
var __VLS_142 = {
    onSearch: (__VLS_ctx.handleSearchResource)
};
var __VLS_143 = {
    onClear: (__VLS_ctx.handleSearchResource)
};
var __VLS_144 = {
    onDropdownReachBottom: (__VLS_ctx.loadMoreResource)
};
__VLS_137.slots.default;
for (var _f = 0, _g = __VLS_getVForSourceType((__VLS_ctx.resourceList)); _f < _g.length; _f++) {
    var resource = _g[_f][0];
    var __VLS_145 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_146 = __VLS_asFunctionalComponent(__VLS_145, new __VLS_145({
        key: (resource.id),
        value: (resource.id),
    }));
    var __VLS_147 = __VLS_146.apply(void 0, __spreadArray([{
            key: (resource.id),
            value: (resource.id),
        }], __VLS_functionalComponentArgsRest(__VLS_146), false));
    __VLS_148.slots.default;
    (resource.name);
    var __VLS_148;
}
var __VLS_137;
var __VLS_133;
var __VLS_129;
var __VLS_149 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_150 = __VLS_asFunctionalComponent(__VLS_149, new __VLS_149({
    span: (12),
}));
var __VLS_151 = __VLS_150.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_150), false));
__VLS_152.slots.default;
var __VLS_153 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_154 = __VLS_asFunctionalComponent(__VLS_153, new __VLS_153({
    field: "permissionIds",
    label: "权限",
}));
var __VLS_155 = __VLS_154.apply(void 0, __spreadArray([{
        field: "permissionIds",
        label: "权限",
    }], __VLS_functionalComponentArgsRest(__VLS_154), false));
__VLS_156.slots.default;
var __VLS_157 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_158 = __VLS_asFunctionalComponent(__VLS_157, new __VLS_157(__assign(__assign(__assign(__assign({ 'onPopupVisibleChange': {} }, { 'onSearch': {} }), { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择权限", multiple: true, allowClear: true, allowSearch: true, modelValue: (__VLS_ctx.authorizeForm.permissionIds), inputValue: (__VLS_ctx.permissionSearchKeyword), filterOption: (false), disabled: (!__VLS_ctx.authorizeForm.resourceId) })));
var __VLS_159 = __VLS_158.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onPopupVisibleChange': {} }, { 'onSearch': {} }), { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择权限", multiple: true, allowClear: true, allowSearch: true, modelValue: (__VLS_ctx.authorizeForm.permissionIds), inputValue: (__VLS_ctx.permissionSearchKeyword), filterOption: (false), disabled: (!__VLS_ctx.authorizeForm.resourceId) })], __VLS_functionalComponentArgsRest(__VLS_158), false));
var __VLS_161;
var __VLS_162;
var __VLS_163;
var __VLS_164 = {
    onPopupVisibleChange: (__VLS_ctx.handlePermissionSelectVisibleChange)
};
var __VLS_165 = {
    onSearch: (__VLS_ctx.handleSearchPermission)
};
var __VLS_166 = {
    onClear: (__VLS_ctx.handleSearchPermission)
};
var __VLS_167 = {
    onDropdownReachBottom: (__VLS_ctx.loadMorePermission)
};
__VLS_160.slots.default;
for (var _h = 0, _j = __VLS_getVForSourceType((__VLS_ctx.permissionList)); _h < _j.length; _h++) {
    var permission = _j[_h][0];
    var __VLS_168 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
        key: (permission.permissionId),
        value: (permission.permissionId),
    }));
    var __VLS_170 = __VLS_169.apply(void 0, __spreadArray([{
            key: (permission.permissionId),
            value: (permission.permissionId),
        }], __VLS_functionalComponentArgsRest(__VLS_169), false));
    __VLS_171.slots.default;
    (permission.permissionName);
    var __VLS_171;
}
var __VLS_160;
var __VLS_156;
var __VLS_152;
var __VLS_172 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172({
    span: (12),
}));
var __VLS_174 = __VLS_173.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_173), false));
__VLS_175.slots.default;
var __VLS_176 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_177 = __VLS_asFunctionalComponent(__VLS_176, new __VLS_176({
    field: "expressionIds",
    label: "限制条件",
}));
var __VLS_178 = __VLS_177.apply(void 0, __spreadArray([{
        field: "expressionIds",
        label: "限制条件",
    }], __VLS_functionalComponentArgsRest(__VLS_177), false));
__VLS_179.slots.default;
var __VLS_180 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择限制条件", multiple: true, allowClear: true, allowSearch: true, modelValue: (__VLS_ctx.authorizeForm.expressionIds), inputValue: (__VLS_ctx.authorizeConditionSearchKeyword), filterOption: (false), disabled: (__VLS_ctx.authorizeForm.permissionIds.length === 0) })));
var __VLS_182 = __VLS_181.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onDropdownReachBottom': {} }), { placeholder: "请选择限制条件", multiple: true, allowClear: true, allowSearch: true, modelValue: (__VLS_ctx.authorizeForm.expressionIds), inputValue: (__VLS_ctx.authorizeConditionSearchKeyword), filterOption: (false), disabled: (__VLS_ctx.authorizeForm.permissionIds.length === 0) })], __VLS_functionalComponentArgsRest(__VLS_181), false));
var __VLS_184;
var __VLS_185;
var __VLS_186;
var __VLS_187 = {
    onSearch: (__VLS_ctx.handleSearchAuthorizeCondition)
};
var __VLS_188 = {
    onClear: (__VLS_ctx.handleSearchAuthorizeCondition)
};
var __VLS_189 = {
    onDropdownReachBottom: (__VLS_ctx.loadMoreAuthorizeCondition)
};
__VLS_183.slots.default;
for (var _k = 0, _l = __VLS_getVForSourceType((__VLS_ctx.authorizeConditionList)); _k < _l.length; _k++) {
    var permission = _l[_k][0];
    var __VLS_190 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_191 = __VLS_asFunctionalComponent(__VLS_190, new __VLS_190({
        key: (permission.id),
        value: (permission.id),
    }));
    var __VLS_192 = __VLS_191.apply(void 0, __spreadArray([{
            key: (permission.id),
            value: (permission.id),
        }], __VLS_functionalComponentArgsRest(__VLS_191), false));
    __VLS_193.slots.default;
    (permission.name);
    var __VLS_193;
}
var __VLS_183;
var __VLS_179;
var __VLS_175;
var __VLS_194 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_195 = __VLS_asFunctionalComponent(__VLS_194, new __VLS_194({
    span: (12),
}));
var __VLS_196 = __VLS_195.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_195), false));
__VLS_197.slots.default;
var __VLS_198 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_199 = __VLS_asFunctionalComponent(__VLS_198, new __VLS_198({
    field: "priority",
    label: "优先级",
}));
var __VLS_200 = __VLS_199.apply(void 0, __spreadArray([{
        field: "priority",
        label: "优先级",
    }], __VLS_functionalComponentArgsRest(__VLS_199), false));
__VLS_201.slots.default;
var __VLS_202 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_203 = __VLS_asFunctionalComponent(__VLS_202, new __VLS_202({
    placeholder: "请选择优先级",
    defaultValue: (0),
    modelValue: (__VLS_ctx.authorizeForm.priority),
}));
var __VLS_204 = __VLS_203.apply(void 0, __spreadArray([{
        placeholder: "请选择优先级",
        defaultValue: (0),
        modelValue: (__VLS_ctx.authorizeForm.priority),
    }], __VLS_functionalComponentArgsRest(__VLS_203), false));
__VLS_205.slots.default;
var __VLS_206 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_207 = __VLS_asFunctionalComponent(__VLS_206, new __VLS_206({
    value: (-1),
}));
var __VLS_208 = __VLS_207.apply(void 0, __spreadArray([{
        value: (-1),
    }], __VLS_functionalComponentArgsRest(__VLS_207), false));
__VLS_209.slots.default;
var __VLS_209;
var __VLS_210 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_211 = __VLS_asFunctionalComponent(__VLS_210, new __VLS_210({
    value: (0),
}));
var __VLS_212 = __VLS_211.apply(void 0, __spreadArray([{
        value: (0),
    }], __VLS_functionalComponentArgsRest(__VLS_211), false));
__VLS_213.slots.default;
var __VLS_213;
var __VLS_214 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_215 = __VLS_asFunctionalComponent(__VLS_214, new __VLS_214({
    value: (1),
}));
var __VLS_216 = __VLS_215.apply(void 0, __spreadArray([{
        value: (1),
    }], __VLS_functionalComponentArgsRest(__VLS_215), false));
__VLS_217.slots.default;
var __VLS_217;
var __VLS_218 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_219 = __VLS_asFunctionalComponent(__VLS_218, new __VLS_218({
    value: (2),
}));
var __VLS_220 = __VLS_219.apply(void 0, __spreadArray([{
        value: (2),
    }], __VLS_functionalComponentArgsRest(__VLS_219), false));
__VLS_221.slots.default;
var __VLS_221;
var __VLS_222 = {}.AOption;
/** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
// @ts-ignore
var __VLS_223 = __VLS_asFunctionalComponent(__VLS_222, new __VLS_222({
    value: (3),
}));
var __VLS_224 = __VLS_223.apply(void 0, __spreadArray([{
        value: (3),
    }], __VLS_functionalComponentArgsRest(__VLS_223), false));
__VLS_225.slots.default;
var __VLS_225;
var __VLS_205;
var __VLS_201;
var __VLS_197;
var __VLS_125;
var __VLS_226 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_227 = __VLS_asFunctionalComponent(__VLS_226, new __VLS_226({
    hideLabel: true,
}));
var __VLS_228 = __VLS_227.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_227), false));
__VLS_229.slots.default;
var __VLS_230 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_231 = __VLS_asFunctionalComponent(__VLS_230, new __VLS_230({}));
var __VLS_232 = __VLS_231.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_231), false));
__VLS_233.slots.default;
var __VLS_234 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_235 = __VLS_asFunctionalComponent(__VLS_234, new __VLS_234({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_236 = __VLS_235.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_235), false));
__VLS_237.slots.default;
var __VLS_237;
var __VLS_238 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_239 = __VLS_asFunctionalComponent(__VLS_238, new __VLS_238(__assign({ 'onClick': {} })));
var __VLS_240 = __VLS_239.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_239), false));
var __VLS_242;
var __VLS_243;
var __VLS_244;
var __VLS_245 = {
    onClick: (__VLS_ctx.handleResetAuthorizeForm)
};
__VLS_241.slots.default;
var __VLS_241;
var __VLS_233;
var __VLS_229;
var __VLS_115;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['create-tile']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['resource-group-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
// @ts-ignore
var __VLS_25 = __VLS_24, __VLS_121 = __VLS_120;
var __VLS_dollars;
var __VLS_self;
