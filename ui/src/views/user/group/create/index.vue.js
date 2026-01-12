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
import createTs from "./index";
export default createTs;
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
    model: (__VLS_ctx.createUserGroupForm),
    ref: "createUserGroupFormRef",
    rules: (__VLS_ctx.createUserGroupFormRules),
    layout: "vertical",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.createUserGroupForm),
        ref: "createUserGroupFormRef",
        rules: (__VLS_ctx.createUserGroupFormRules),
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
/** @type {typeof __VLS_ctx.createUserGroupFormRef} */ ;
var __VLS_12 = {};
__VLS_11.slots.default;
var __VLS_14 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_15 = __VLS_asFunctionalComponent(__VLS_14, new __VLS_14({
    gutter: (24),
}));
var __VLS_16 = __VLS_15.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_15), false));
__VLS_17.slots.default;
var __VLS_18 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18({
    span: (12),
}));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_19), false));
__VLS_21.slots.default;
var __VLS_22 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_23 = __VLS_asFunctionalComponent(__VLS_22, new __VLS_22({
    field: "name",
    label: "用户组名称",
}));
var __VLS_24 = __VLS_23.apply(void 0, __spreadArray([{
        field: "name",
        label: "用户组名称",
    }], __VLS_functionalComponentArgsRest(__VLS_23), false));
__VLS_25.slots.default;
var __VLS_26 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26({
    modelValue: (__VLS_ctx.createUserGroupForm.name),
    placeholder: "请输入用户组名称",
}));
var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createUserGroupForm.name),
        placeholder: "请输入用户组名称",
    }], __VLS_functionalComponentArgsRest(__VLS_27), false));
var __VLS_25;
var __VLS_21;
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
    field: "code",
    label: "用户组标识",
}));
var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
        field: "code",
        label: "用户组标识",
    }], __VLS_functionalComponentArgsRest(__VLS_35), false));
__VLS_37.slots.default;
var __VLS_38 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
    modelValue: (__VLS_ctx.createUserGroupForm.code),
    placeholder: "请输入用户组标识",
}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createUserGroupForm.code),
        placeholder: "请输入用户组标识",
    }], __VLS_functionalComponentArgsRest(__VLS_39), false));
var __VLS_37;
var __VLS_33;
var __VLS_42 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    span: (12),
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    field: "type",
    label: "用户组类型",
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        field: "type",
        label: "用户组类型",
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
__VLS_49.slots.default;
var __VLS_50 = {}.ARadioGroup;
/** @type {[typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, typeof __VLS_components.ARadioGroup, typeof __VLS_components.aRadioGroup, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50(__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.createUserGroupForm.type) })));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { modelValue: (__VLS_ctx.createUserGroupForm.type) })], __VLS_functionalComponentArgsRest(__VLS_51), false));
var __VLS_54;
var __VLS_55;
var __VLS_56;
var __VLS_57 = {
    onChange: (__VLS_ctx.handleUserGroupTypeChange)
};
__VLS_53.slots.default;
var __VLS_58 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
    value: "STATIC",
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        value: "STATIC",
    }], __VLS_functionalComponentArgsRest(__VLS_59), false));
__VLS_61.slots.default;
var __VLS_61;
var __VLS_62 = {}.ARadio;
/** @type {[typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, typeof __VLS_components.ARadio, typeof __VLS_components.aRadio, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    value: "DYNAMIC",
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        value: "DYNAMIC",
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
__VLS_65.slots.default;
var __VLS_65;
var __VLS_53;
var __VLS_49;
var __VLS_45;
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
    field: "desc",
    label: "用户组描述",
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        field: "desc",
        label: "用户组描述",
    }], __VLS_functionalComponentArgsRest(__VLS_71), false));
__VLS_73.slots.default;
var __VLS_74 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    modelValue: (__VLS_ctx.createUserGroupForm.desc),
    placeholder: "请输入用户组描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.createUserGroupForm.desc),
        placeholder: "请输入用户组描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
var __VLS_73;
var __VLS_69;
var __VLS_17;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
if (__VLS_ctx.createUserGroupForm.type === 'DYNAMIC') {
    var __VLS_78 = {}.UserGroupConditions;
    /** @type {[typeof __VLS_components.UserGroupConditions, ]} */ ;
    // @ts-ignore
    var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
        conditions: (__VLS_ctx.createUserGroupForm.conditions),
        ref: "userGroupConditionsRef",
    }));
    var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
            conditions: (__VLS_ctx.createUserGroupForm.conditions),
            ref: "userGroupConditionsRef",
        }], __VLS_functionalComponentArgsRest(__VLS_79), false));
    /** @type {typeof __VLS_ctx.userGroupConditionsRef} */ ;
    var __VLS_82 = {};
    var __VLS_81;
}
var __VLS_84 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
    hideLabel: true,
}));
var __VLS_86 = __VLS_85.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_85), false));
__VLS_87.slots.default;
var __VLS_88 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88({}));
var __VLS_90 = __VLS_89.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_89), false));
__VLS_91.slots.default;
var __VLS_92 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_94 = __VLS_93.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_93), false));
var __VLS_96;
var __VLS_97;
var __VLS_98;
var __VLS_99 = {
    onClick: (__VLS_ctx.handleCreateUserGroupFormSubmit)
};
__VLS_95.slots.default;
var __VLS_95;
var __VLS_100 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100(__assign({ 'onClick': {} })));
var __VLS_102 = __VLS_101.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_101), false));
var __VLS_104;
var __VLS_105;
var __VLS_106;
var __VLS_107 = {
    onClick: (__VLS_ctx.handleResetCreateUserGroupForm)
};
__VLS_103.slots.default;
var __VLS_103;
var __VLS_91;
var __VLS_87;
var __VLS_11;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['create-tile']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
// @ts-ignore
var __VLS_13 = __VLS_12, __VLS_83 = __VLS_82;
var __VLS_dollars;
var __VLS_self;
