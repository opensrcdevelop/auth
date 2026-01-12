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
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "title" }));
(__VLS_ctx.roleName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.roleId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.roleId),
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
    key: "role_info",
    title: "角色信息",
}));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{
        key: "role_info",
        title: "角色信息",
    }], __VLS_functionalComponentArgsRest(__VLS_21), false));
__VLS_23.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_24 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.roleInfoForm), rules: (__VLS_ctx.roleInfoFormRules), ref: "roleInfoFormRef", layout: "vertical" })));
var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.roleInfoForm), rules: (__VLS_ctx.roleInfoFormRules), ref: "roleInfoFormRef", layout: "vertical" })], __VLS_functionalComponentArgsRest(__VLS_25), false));
var __VLS_28;
var __VLS_29;
var __VLS_30;
var __VLS_31 = {
    onSubmitSuccess: (__VLS_ctx.handleRoleInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.roleInfoFormRef} */ ;
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
    label: "角色名称",
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        field: "name",
        label: "角色名称",
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    modelValue: (__VLS_ctx.roleInfoForm.name),
    placeholder: "请输入角色名称",
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.roleInfoForm.name),
        placeholder: "请输入角色名称",
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
    label: "角色标识",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        field: "code",
        label: "角色标识",
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
__VLS_57.slots.default;
var __VLS_58 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
    modelValue: (__VLS_ctx.roleInfoForm.code),
    placeholder: "请输入用户组标识",
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.roleInfoForm.code),
        placeholder: "请输入用户组标识",
    }], __VLS_functionalComponentArgsRest(__VLS_59), false));
var __VLS_57;
var __VLS_53;
var __VLS_37;
var __VLS_62 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    field: "desc",
    label: "角色描述",
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        field: "desc",
        label: "角色描述",
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
__VLS_65.slots.default;
var __VLS_66 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
    modelValue: (__VLS_ctx.roleInfoForm.desc),
    placeholder: "请输入角色描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.roleInfoForm.desc),
        placeholder: "请输入角色描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
var __VLS_65;
var __VLS_70 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
    hideLabel: true,
}));
var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_71), false));
__VLS_73.slots.default;
var __VLS_74 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_75), false));
__VLS_77.slots.default;
var __VLS_78 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_81;
var __VLS_82 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82(__assign({ 'onClick': {} })));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_83), false));
var __VLS_86;
var __VLS_87;
var __VLS_88;
var __VLS_89 = {
    onClick: (__VLS_ctx.handleResetRoleInfoForm)
};
__VLS_85.slots.default;
var __VLS_85;
var __VLS_77;
var __VLS_73;
var __VLS_27;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tools-container" }));
var __VLS_90 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onKeyup': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入用户名 / 用户组名进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchRolePrincipalKeyword) })));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onKeyup': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入用户名 / 用户组名进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchRolePrincipalKeyword) })], __VLS_functionalComponentArgsRest(__VLS_91), false));
var __VLS_94;
var __VLS_95;
var __VLS_96;
var __VLS_97 = {
    onSearch: (__VLS_ctx.handleSearchRolePrincipal)
};
var __VLS_98 = {
    onClear: (__VLS_ctx.handleSearchRolePrincipal)
};
var __VLS_99 = {
    onKeyup: (__VLS_ctx.handleSearchRolePrincipal)
};
var __VLS_93;
var __VLS_100 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100(__assign({ 'onClick': {} }, { type: "text" })));
var __VLS_102 = __VLS_101.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text" })], __VLS_functionalComponentArgsRest(__VLS_101), false));
var __VLS_104;
var __VLS_105;
var __VLS_106;
var __VLS_107 = {
    onClick: (__VLS_ctx.handleOpenAddRolePrincipalModel)
};
__VLS_103.slots.default;
{
    var __VLS_thisSlot = __VLS_103.slots.icon;
    var __VLS_108 = {}.IconPlus;
    /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
    // @ts-ignore
    var __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({}));
    var __VLS_110 = __VLS_109.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_109), false));
}
var __VLS_103;
var __VLS_112 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112(__assign(__assign({ 'onPageSizeChange': {} }, { 'onPageChange': {} }), { data: (__VLS_ctx.rolePrincipals), pagination: (__VLS_ctx.rolePrincipalsPagination.pagination), bordered: (false), scroll: ({ y: '100%' }) })));
var __VLS_114 = __VLS_113.apply(void 0, __spreadArray([__assign(__assign({ 'onPageSizeChange': {} }, { 'onPageChange': {} }), { data: (__VLS_ctx.rolePrincipals), pagination: (__VLS_ctx.rolePrincipalsPagination.pagination), bordered: (false), scroll: ({ y: '100%' }) })], __VLS_functionalComponentArgsRest(__VLS_113), false));
var __VLS_116;
var __VLS_117;
var __VLS_118;
var __VLS_119 = {
    onPageSizeChange: (__VLS_ctx.rolePrincipalsPagination.handlePageSizeChange)
};
var __VLS_120 = {
    onPageChange: (__VLS_ctx.rolePrincipalsPagination.handlePageChange)
};
__VLS_115.slots.default;
{
    var __VLS_thisSlot = __VLS_115.slots.columns;
    var __VLS_121 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_122 = __VLS_asFunctionalComponent(__VLS_121, new __VLS_121({
        title: "主体名称",
    }));
    var __VLS_123 = __VLS_122.apply(void 0, __spreadArray([{
            title: "主体名称",
        }], __VLS_functionalComponentArgsRest(__VLS_122), false));
    __VLS_124.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_124.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToPrincipalDetail(record_1);
            } }, { class: "table-column-name" }));
        (record_1.principal);
    }
    var __VLS_124;
    var __VLS_125 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_126 = __VLS_asFunctionalComponent(__VLS_125, new __VLS_125({
        title: "主体类型",
    }));
    var __VLS_127 = __VLS_126.apply(void 0, __spreadArray([{
            title: "主体类型",
        }], __VLS_functionalComponentArgsRest(__VLS_126), false));
    __VLS_128.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_128.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        (record.principalTypeDisplayName);
    }
    var __VLS_128;
    var __VLS_129 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_130 = __VLS_asFunctionalComponent(__VLS_129, new __VLS_129({
        title: "操作",
        width: (60),
    }));
    var __VLS_131 = __VLS_130.apply(void 0, __spreadArray([{
            title: "操作",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_130), false));
    __VLS_132.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_132.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        var __VLS_133 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_134 = __VLS_asFunctionalComponent(__VLS_133, new __VLS_133({}));
        var __VLS_135 = __VLS_134.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_134), false));
        __VLS_136.slots.default;
        var __VLS_137 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_138 = __VLS_asFunctionalComponent(__VLS_137, new __VLS_137({
            type: "text",
        }));
        var __VLS_139 = __VLS_138.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_138), false));
        __VLS_140.slots.default;
        {
            var __VLS_thisSlot_4 = __VLS_140.slots.icon;
            var __VLS_141 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_142 = __VLS_asFunctionalComponent(__VLS_141, new __VLS_141({}));
            var __VLS_143 = __VLS_142.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_142), false));
        }
        var __VLS_140;
        {
            var __VLS_thisSlot_5 = __VLS_136.slots.content;
            var __VLS_145 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_146 = __VLS_asFunctionalComponent(__VLS_145, new __VLS_145(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_147 = __VLS_146.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_146), false));
            var __VLS_149 = void 0;
            var __VLS_150 = void 0;
            var __VLS_151 = void 0;
            var __VLS_152 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleRemoveRolePrincipal(record_2);
                }
            };
            __VLS_148.slots.default;
            {
                var __VLS_thisSlot_6 = __VLS_148.slots.icon;
                var __VLS_153 = {}.IconUndo;
                /** @type {[typeof __VLS_components.IconUndo, typeof __VLS_components.iconUndo, ]} */ ;
                // @ts-ignore
                var __VLS_154 = __VLS_asFunctionalComponent(__VLS_153, new __VLS_153({}));
                var __VLS_155 = __VLS_154.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_154), false));
            }
            var __VLS_148;
        }
        var __VLS_136;
    }
    var __VLS_132;
}
var __VLS_115;
var __VLS_23;
var __VLS_157 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_158 = __VLS_asFunctionalComponent(__VLS_157, new __VLS_157({
    key: "permission_management",
    title: "权限管理",
}));
var __VLS_159 = __VLS_158.apply(void 0, __spreadArray([{
        key: "permission_management",
        title: "权限管理",
    }], __VLS_functionalComponentArgsRest(__VLS_158), false));
__VLS_160.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "add-container" }));
var __VLS_161 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_162 = __VLS_asFunctionalComponent(__VLS_161, new __VLS_161(__assign({ 'onClick': {} }, { type: "text" })));
var __VLS_163 = __VLS_162.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text" })], __VLS_functionalComponentArgsRest(__VLS_162), false));
var __VLS_165;
var __VLS_166;
var __VLS_167;
var __VLS_168 = {
    onClick: (__VLS_ctx.handleAuthorize)
};
__VLS_164.slots.default;
{
    var __VLS_thisSlot = __VLS_164.slots.icon;
    var __VLS_169 = {}.IconPlus;
    /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
    // @ts-ignore
    var __VLS_170 = __VLS_asFunctionalComponent(__VLS_169, new __VLS_169({}));
    var __VLS_171 = __VLS_170.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_170), false));
}
var __VLS_164;
var __VLS_173 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_174 = __VLS_asFunctionalComponent(__VLS_173, new __VLS_173(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.permissions), bordered: (false), pagination: (__VLS_ctx.permissionsPagination.pagination), expandable: ({ width: 30 }), rowKey: "authorizeId" })));
var __VLS_175 = __VLS_174.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.permissions), bordered: (false), pagination: (__VLS_ctx.permissionsPagination.pagination), expandable: ({ width: 30 }), rowKey: "authorizeId" })], __VLS_functionalComponentArgsRest(__VLS_174), false));
var __VLS_177;
var __VLS_178;
var __VLS_179;
var __VLS_180 = {
    onPageChange: (__VLS_ctx.permissionsPagination.handlePageChange)
};
var __VLS_181 = {
    onPageSizeChange: (__VLS_ctx.permissionsPagination.handlePageSizeChange)
};
__VLS_176.slots.default;
{
    var __VLS_thisSlot = __VLS_176.slots.columns;
    var __VLS_182 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_183 = __VLS_asFunctionalComponent(__VLS_182, new __VLS_182({
        title: "资源组",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
        filterable: (__VLS_ctx.resourceGroupNameFilter),
    }));
    var __VLS_184 = __VLS_183.apply(void 0, __spreadArray([{
            title: "资源组",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
            filterable: (__VLS_ctx.resourceGroupNameFilter),
        }], __VLS_functionalComponentArgsRest(__VLS_183), false));
    __VLS_185.slots.default;
    {
        var __VLS_thisSlot_7 = __VLS_185.slots.cell;
        var record_3 = __VLS_getSlotParams(__VLS_thisSlot_7)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToResourceGroupDetail(record_3.resourceGroupId);
            } }, { class: "table-column-name" }));
        (record_3.resourceGroupName);
    }
    var __VLS_185;
    var __VLS_186 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_187 = __VLS_asFunctionalComponent(__VLS_186, new __VLS_186({
        title: "资源",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
        filterable: (__VLS_ctx.resourceNameFilter),
    }));
    var __VLS_188 = __VLS_187.apply(void 0, __spreadArray([{
            title: "资源",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
            filterable: (__VLS_ctx.resourceNameFilter),
        }], __VLS_functionalComponentArgsRest(__VLS_187), false));
    __VLS_189.slots.default;
    {
        var __VLS_thisSlot_8 = __VLS_189.slots.cell;
        var record_4 = __VLS_getSlotParams(__VLS_thisSlot_8)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToResourceDetail(record_4.resourceId);
            } }, { class: "table-column-name" }));
        (record_4.resourceName);
    }
    var __VLS_189;
    var __VLS_190 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_191 = __VLS_asFunctionalComponent(__VLS_190, new __VLS_190({
        title: "权限名称",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
        filterable: (__VLS_ctx.permissionNameFilter),
    }));
    var __VLS_192 = __VLS_191.apply(void 0, __spreadArray([{
            title: "权限名称",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
            filterable: (__VLS_ctx.permissionNameFilter),
        }], __VLS_functionalComponentArgsRest(__VLS_191), false));
    __VLS_193.slots.default;
    {
        var __VLS_thisSlot_9 = __VLS_193.slots.cell;
        var record_5 = __VLS_getSlotParams(__VLS_thisSlot_9)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToPermissionDetail(record_5.permissionId);
            } }, { class: "table-column-name" }));
        (record_5.permissionName);
    }
    var __VLS_193;
    var __VLS_194 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_195 = __VLS_asFunctionalComponent(__VLS_194, new __VLS_194({
        title: "权限标识",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
        filterable: (__VLS_ctx.permissionCodeFilter),
    }));
    var __VLS_196 = __VLS_195.apply(void 0, __spreadArray([{
            title: "权限标识",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
            filterable: (__VLS_ctx.permissionCodeFilter),
        }], __VLS_functionalComponentArgsRest(__VLS_195), false));
    __VLS_197.slots.default;
    {
        var __VLS_thisSlot_10 = __VLS_197.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_10)[0].record;
        (record.permissionCode);
    }
    var __VLS_197;
    var __VLS_198 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_199 = __VLS_asFunctionalComponent(__VLS_198, new __VLS_198({
        title: "优先级",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_200 = __VLS_199.apply(void 0, __spreadArray([{
            title: "优先级",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_199), false));
    __VLS_201.slots.default;
    {
        var __VLS_thisSlot_11 = __VLS_201.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_11)[0].record;
        var __VLS_202 = {}.PriorityTag;
        /** @type {[typeof __VLS_components.PriorityTag, typeof __VLS_components.priorityTag, ]} */ ;
        // @ts-ignore
        var __VLS_203 = __VLS_asFunctionalComponent(__VLS_202, new __VLS_202({
            priority: (record.priority),
        }));
        var __VLS_204 = __VLS_203.apply(void 0, __spreadArray([{
                priority: (record.priority),
            }], __VLS_functionalComponentArgsRest(__VLS_203), false));
    }
    var __VLS_201;
    var __VLS_206 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_207 = __VLS_asFunctionalComponent(__VLS_206, new __VLS_206({
        title: "操作",
    }));
    var __VLS_208 = __VLS_207.apply(void 0, __spreadArray([{
            title: "操作",
        }], __VLS_functionalComponentArgsRest(__VLS_207), false));
    __VLS_209.slots.default;
    {
        var __VLS_thisSlot_12 = __VLS_209.slots.cell;
        var record_6 = __VLS_getSlotParams(__VLS_thisSlot_12)[0].record;
        var __VLS_210 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_211 = __VLS_asFunctionalComponent(__VLS_210, new __VLS_210({}));
        var __VLS_212 = __VLS_211.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_211), false));
        __VLS_213.slots.default;
        var __VLS_214 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_215 = __VLS_asFunctionalComponent(__VLS_214, new __VLS_214({
            type: "text",
        }));
        var __VLS_216 = __VLS_215.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_215), false));
        __VLS_217.slots.default;
        {
            var __VLS_thisSlot_13 = __VLS_217.slots.icon;
            var __VLS_218 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_219 = __VLS_asFunctionalComponent(__VLS_218, new __VLS_218({}));
            var __VLS_220 = __VLS_219.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_219), false));
        }
        var __VLS_217;
        {
            var __VLS_thisSlot_14 = __VLS_213.slots.content;
            var __VLS_222 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_223 = __VLS_asFunctionalComponent(__VLS_222, new __VLS_222(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_224 = __VLS_223.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_223), false));
            var __VLS_226 = void 0;
            var __VLS_227 = void 0;
            var __VLS_228 = void 0;
            var __VLS_229 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleCancelAuthorization(record_6);
                }
            };
            __VLS_225.slots.default;
            {
                var __VLS_thisSlot_15 = __VLS_225.slots.icon;
                var __VLS_230 = {}.IconUndo;
                /** @type {[typeof __VLS_components.IconUndo, typeof __VLS_components.iconUndo, ]} */ ;
                // @ts-ignore
                var __VLS_231 = __VLS_asFunctionalComponent(__VLS_230, new __VLS_230({}));
                var __VLS_232 = __VLS_231.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_231), false));
            }
            var __VLS_225;
        }
        var __VLS_213;
    }
    var __VLS_209;
}
{
    var __VLS_thisSlot = __VLS_176.slots["expand-row"];
    var record = __VLS_getSlotParams(__VLS_thisSlot)[0].record;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "condition-container" }));
    if (record.conditions.length > 0) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    }
    else {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    }
    if (record.conditions.length > 0) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
        for (var _i = 0, _a = __VLS_getVForSourceType((record.conditions)); _i < _a.length; _i++) {
            var _b = _a[_i], condition = _b[0], index = _b[1];
            var __VLS_234 = {}.ADescriptions;
            /** @type {[typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, ]} */ ;
            // @ts-ignore
            var __VLS_235 = __VLS_asFunctionalComponent(__VLS_234, new __VLS_234(__assign({ style: {} }, { key: (index), column: (1), bordered: true })));
            var __VLS_236 = __VLS_235.apply(void 0, __spreadArray([__assign({ style: {} }, { key: (index), column: (1), bordered: true })], __VLS_functionalComponentArgsRest(__VLS_235), false));
            __VLS_237.slots.default;
            var __VLS_238 = {}.ADescriptionsItem;
            /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
            // @ts-ignore
            var __VLS_239 = __VLS_asFunctionalComponent(__VLS_238, new __VLS_238({
                label: "限制条件名称",
            }));
            var __VLS_240 = __VLS_239.apply(void 0, __spreadArray([{
                    label: "限制条件名称",
                }], __VLS_functionalComponentArgsRest(__VLS_239), false));
            __VLS_241.slots.default;
            var __VLS_242 = {}.ALink;
            /** @type {[typeof __VLS_components.ALink, typeof __VLS_components.aLink, typeof __VLS_components.ALink, typeof __VLS_components.aLink, ]} */ ;
            // @ts-ignore
            var __VLS_243 = __VLS_asFunctionalComponent(__VLS_242, new __VLS_242({}));
            var __VLS_244 = __VLS_243.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_243), false));
            __VLS_245.slots.default;
            __VLS_asFunctionalElement(__VLS_intrinsicElements.a, __VLS_intrinsicElements.a)({
                href: ("/ui/permission/expression/detail?id=".concat(condition.id)),
                target: "_blank",
            });
            (condition.name);
            var __VLS_246 = {}.IconLaunch;
            /** @type {[typeof __VLS_components.IconLaunch, typeof __VLS_components.iconLaunch, ]} */ ;
            // @ts-ignore
            var __VLS_247 = __VLS_asFunctionalComponent(__VLS_246, new __VLS_246(__assign({ style: {} })));
            var __VLS_248 = __VLS_247.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_247), false));
            var __VLS_245;
            var __VLS_241;
            var __VLS_250 = {}.ADescriptionsItem;
            /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
            // @ts-ignore
            var __VLS_251 = __VLS_asFunctionalComponent(__VLS_250, new __VLS_250({
                label: "限制条件描述",
            }));
            var __VLS_252 = __VLS_251.apply(void 0, __spreadArray([{
                    label: "限制条件描述",
                }], __VLS_functionalComponentArgsRest(__VLS_251), false));
            __VLS_253.slots.default;
            (condition.desc ? condition.desc : "-");
            var __VLS_253;
            var __VLS_237;
        }
    }
}
{
    var __VLS_thisSlot = __VLS_176.slots["expand-icon"];
    var expanded = __VLS_getSlotParams(__VLS_thisSlot)[0].expanded;
    if (!expanded) {
        var __VLS_254 = {}.IconRight;
        /** @type {[typeof __VLS_components.IconRight, typeof __VLS_components.iconRight, ]} */ ;
        // @ts-ignore
        var __VLS_255 = __VLS_asFunctionalComponent(__VLS_254, new __VLS_254({}));
        var __VLS_256 = __VLS_255.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_255), false));
    }
    if (expanded) {
        var __VLS_258 = {}.IconDown;
        /** @type {[typeof __VLS_components.IconDown, typeof __VLS_components.iconDown, ]} */ ;
        // @ts-ignore
        var __VLS_259 = __VLS_asFunctionalComponent(__VLS_258, new __VLS_258({}));
        var __VLS_260 = __VLS_259.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_259), false));
    }
}
{
    var __VLS_thisSlot = __VLS_176.slots["resource-group-name-filter"];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "permission-filter" }));
    var __VLS_262 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_263 = __VLS_asFunctionalComponent(__VLS_262, new __VLS_262({
        placeholder: "输入资源组名称进行搜索",
        modelValue: (__VLS_ctx.authorizeSearchKeywords.resourceGroupName),
    }));
    var __VLS_264 = __VLS_263.apply(void 0, __spreadArray([{
            placeholder: "输入资源组名称进行搜索",
            modelValue: (__VLS_ctx.authorizeSearchKeywords.resourceGroupName),
        }], __VLS_functionalComponentArgsRest(__VLS_263), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "filter-footer" }));
    var __VLS_266 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_267 = __VLS_asFunctionalComponent(__VLS_266, new __VLS_266({}));
    var __VLS_268 = __VLS_267.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_267), false));
    __VLS_269.slots.default;
    var __VLS_270 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_271 = __VLS_asFunctionalComponent(__VLS_270, new __VLS_270(__assign({ 'onClick': {} }, { type: "primary" })));
    var __VLS_272 = __VLS_271.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_271), false));
    var __VLS_274 = void 0;
    var __VLS_275 = void 0;
    var __VLS_276 = void 0;
    var __VLS_277 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleGetRolePermissions();
        }
    };
    __VLS_273.slots.default;
    var __VLS_273;
    var __VLS_278 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_279 = __VLS_asFunctionalComponent(__VLS_278, new __VLS_278(__assign({ 'onClick': {} })));
    var __VLS_280 = __VLS_279.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_279), false));
    var __VLS_282 = void 0;
    var __VLS_283 = void 0;
    var __VLS_284 = void 0;
    var __VLS_285 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleResetPermissionFilter('resourceGroupName');
        }
    };
    __VLS_281.slots.default;
    var __VLS_281;
    var __VLS_269;
}
{
    var __VLS_thisSlot = __VLS_176.slots["resource-name-filter"];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "permission-filter" }));
    var __VLS_286 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_287 = __VLS_asFunctionalComponent(__VLS_286, new __VLS_286({
        placeholder: "输入资源名称进行搜索",
        modelValue: (__VLS_ctx.authorizeSearchKeywords.resourceName),
    }));
    var __VLS_288 = __VLS_287.apply(void 0, __spreadArray([{
            placeholder: "输入资源名称进行搜索",
            modelValue: (__VLS_ctx.authorizeSearchKeywords.resourceName),
        }], __VLS_functionalComponentArgsRest(__VLS_287), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "filter-footer" }));
    var __VLS_290 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_291 = __VLS_asFunctionalComponent(__VLS_290, new __VLS_290({}));
    var __VLS_292 = __VLS_291.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_291), false));
    __VLS_293.slots.default;
    var __VLS_294 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_295 = __VLS_asFunctionalComponent(__VLS_294, new __VLS_294(__assign({ 'onClick': {} }, { type: "primary" })));
    var __VLS_296 = __VLS_295.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_295), false));
    var __VLS_298 = void 0;
    var __VLS_299 = void 0;
    var __VLS_300 = void 0;
    var __VLS_301 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleGetRolePermissions();
        }
    };
    __VLS_297.slots.default;
    var __VLS_297;
    var __VLS_302 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_303 = __VLS_asFunctionalComponent(__VLS_302, new __VLS_302(__assign({ 'onClick': {} })));
    var __VLS_304 = __VLS_303.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_303), false));
    var __VLS_306 = void 0;
    var __VLS_307 = void 0;
    var __VLS_308 = void 0;
    var __VLS_309 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleResetPermissionFilter('resourceName');
        }
    };
    __VLS_305.slots.default;
    var __VLS_305;
    var __VLS_293;
}
{
    var __VLS_thisSlot = __VLS_176.slots["permission-name-filter"];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "permission-filter" }));
    var __VLS_310 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_311 = __VLS_asFunctionalComponent(__VLS_310, new __VLS_310({
        placeholder: "输入权限名称进行搜索",
        modelValue: (__VLS_ctx.authorizeSearchKeywords.permissionName),
    }));
    var __VLS_312 = __VLS_311.apply(void 0, __spreadArray([{
            placeholder: "输入权限名称进行搜索",
            modelValue: (__VLS_ctx.authorizeSearchKeywords.permissionName),
        }], __VLS_functionalComponentArgsRest(__VLS_311), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "filter-footer" }));
    var __VLS_314 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_315 = __VLS_asFunctionalComponent(__VLS_314, new __VLS_314({}));
    var __VLS_316 = __VLS_315.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_315), false));
    __VLS_317.slots.default;
    var __VLS_318 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_319 = __VLS_asFunctionalComponent(__VLS_318, new __VLS_318(__assign({ 'onClick': {} }, { type: "primary" })));
    var __VLS_320 = __VLS_319.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_319), false));
    var __VLS_322 = void 0;
    var __VLS_323 = void 0;
    var __VLS_324 = void 0;
    var __VLS_325 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleGetRolePermissions();
        }
    };
    __VLS_321.slots.default;
    var __VLS_321;
    var __VLS_326 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_327 = __VLS_asFunctionalComponent(__VLS_326, new __VLS_326(__assign({ 'onClick': {} })));
    var __VLS_328 = __VLS_327.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_327), false));
    var __VLS_330 = void 0;
    var __VLS_331 = void 0;
    var __VLS_332 = void 0;
    var __VLS_333 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleResetPermissionFilter('permissionName');
        }
    };
    __VLS_329.slots.default;
    var __VLS_329;
    var __VLS_317;
}
{
    var __VLS_thisSlot = __VLS_176.slots["permission-code-filter"];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "permission-filter" }));
    var __VLS_334 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_335 = __VLS_asFunctionalComponent(__VLS_334, new __VLS_334({
        placeholder: "输入权限标识进行搜索",
        modelValue: (__VLS_ctx.authorizeSearchKeywords.permissionCode),
    }));
    var __VLS_336 = __VLS_335.apply(void 0, __spreadArray([{
            placeholder: "输入权限标识进行搜索",
            modelValue: (__VLS_ctx.authorizeSearchKeywords.permissionCode),
        }], __VLS_functionalComponentArgsRest(__VLS_335), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "filter-footer" }));
    var __VLS_338 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_339 = __VLS_asFunctionalComponent(__VLS_338, new __VLS_338({}));
    var __VLS_340 = __VLS_339.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_339), false));
    __VLS_341.slots.default;
    var __VLS_342 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_343 = __VLS_asFunctionalComponent(__VLS_342, new __VLS_342(__assign({ 'onClick': {} }, { type: "primary" })));
    var __VLS_344 = __VLS_343.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_343), false));
    var __VLS_346 = void 0;
    var __VLS_347 = void 0;
    var __VLS_348 = void 0;
    var __VLS_349 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleGetRolePermissions();
        }
    };
    __VLS_345.slots.default;
    var __VLS_345;
    var __VLS_350 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_351 = __VLS_asFunctionalComponent(__VLS_350, new __VLS_350(__assign({ 'onClick': {} })));
    var __VLS_352 = __VLS_351.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_351), false));
    var __VLS_354 = void 0;
    var __VLS_355 = void 0;
    var __VLS_356 = void 0;
    var __VLS_357 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleResetPermissionFilter('permissionCode');
        }
    };
    __VLS_353.slots.default;
    var __VLS_353;
    var __VLS_341;
}
var __VLS_176;
var __VLS_160;
var __VLS_15;
var __VLS_3;
var __VLS_358 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_359 = __VLS_asFunctionalComponent(__VLS_358, new __VLS_358(__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.addRolePrincipalModelVisible), footer: (false), width: (728) })));
var __VLS_360 = __VLS_359.apply(void 0, __spreadArray([__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.addRolePrincipalModelVisible), footer: (false), width: (728) })], __VLS_functionalComponentArgsRest(__VLS_359), false));
var __VLS_362;
var __VLS_363;
var __VLS_364;
var __VLS_365 = {
    onCancel: (__VLS_ctx.handleCloseAddRolePrincipalModel)
};
__VLS_361.slots.default;
{
    var __VLS_thisSlot = __VLS_361.slots.title;
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-principal-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-principal-source" }));
var __VLS_366 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_367 = __VLS_asFunctionalComponent(__VLS_366, new __VLS_366({
    modelValue: (__VLS_ctx.activePrincipalTabKey),
}));
var __VLS_368 = __VLS_367.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.activePrincipalTabKey),
    }], __VLS_functionalComponentArgsRest(__VLS_367), false));
__VLS_369.slots.default;
var __VLS_370 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_371 = __VLS_asFunctionalComponent(__VLS_370, new __VLS_370({
    key: "1",
    title: "用户",
}));
var __VLS_372 = __VLS_371.apply(void 0, __spreadArray([{
        key: "1",
        title: "用户",
    }], __VLS_functionalComponentArgsRest(__VLS_371), false));
__VLS_373.slots.default;
var __VLS_374 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_375 = __VLS_asFunctionalComponent(__VLS_374, new __VLS_374(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { class: "search" }), { placeholder: "输入用户名进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchSelectUserKeyword) })));
var __VLS_376 = __VLS_375.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { class: "search" }), { placeholder: "输入用户名进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchSelectUserKeyword) })], __VLS_functionalComponentArgsRest(__VLS_375), false));
var __VLS_378;
var __VLS_379;
var __VLS_380;
var __VLS_381 = {
    onSearch: (__VLS_ctx.handleSearchUser)
};
var __VLS_382 = {
    onKeyup: (__VLS_ctx.handleSearchUser)
};
var __VLS_377;
if (__VLS_ctx.allUsers.length > 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "check-all-container" }));
    var __VLS_383 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_384 = __VLS_asFunctionalComponent(__VLS_383, new __VLS_383(__assign(__assign({ 'onChange': {} }, { style: {} }), { modelValue: (__VLS_ctx.selectUserCheckAll), indeterminate: (__VLS_ctx.selectUserIndeterminate) })));
    var __VLS_385 = __VLS_384.apply(void 0, __spreadArray([__assign(__assign({ 'onChange': {} }, { style: {} }), { modelValue: (__VLS_ctx.selectUserCheckAll), indeterminate: (__VLS_ctx.selectUserIndeterminate) })], __VLS_functionalComponentArgsRest(__VLS_384), false));
    var __VLS_387 = void 0;
    var __VLS_388 = void 0;
    var __VLS_389 = void 0;
    var __VLS_390 = {
        onChange: (__VLS_ctx.handleChangeSelectUserCheckAll)
    };
    __VLS_386.slots.default;
    var __VLS_386;
}
if (__VLS_ctx.allUsers.length === 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "empty-container" }));
    var __VLS_391 = {}.AEmpty;
    /** @type {[typeof __VLS_components.AEmpty, typeof __VLS_components.aEmpty, ]} */ ;
    // @ts-ignore
    var __VLS_392 = __VLS_asFunctionalComponent(__VLS_391, new __VLS_391({}));
    var __VLS_393 = __VLS_392.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_392), false));
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign(__assign({ onScroll: (__VLS_ctx.handleAllUsersContainerScroll) }, { class: "scroll-container" }), { ref: "allUsersContainerRef" }));
    /** @type {typeof __VLS_ctx.allUsersContainerRef} */ ;
    var __VLS_395 = {}.ACheckboxGroup;
    /** @type {[typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, ]} */ ;
    // @ts-ignore
    var __VLS_396 = __VLS_asFunctionalComponent(__VLS_395, new __VLS_395(__assign(__assign(__assign({ 'onChange': {} }, { direction: "vertical" }), { style: {} }), { modelValue: (__VLS_ctx.addRolePrincipalsForm.userIds) })));
    var __VLS_397 = __VLS_396.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onChange': {} }, { direction: "vertical" }), { style: {} }), { modelValue: (__VLS_ctx.addRolePrincipalsForm.userIds) })], __VLS_functionalComponentArgsRest(__VLS_396), false));
    var __VLS_399 = void 0;
    var __VLS_400 = void 0;
    var __VLS_401 = void 0;
    var __VLS_402 = {
        onChange: (__VLS_ctx.handleSelectUserChange)
    };
    __VLS_398.slots.default;
    for (var _c = 0, _d = __VLS_getVForSourceType((__VLS_ctx.allUsers)); _c < _d.length; _c++) {
        var _e = _d[_c], user = _e[0], index = _e[1];
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-user-item" }));
        var __VLS_403 = {}.ACheckbox;
        /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
        // @ts-ignore
        var __VLS_404 = __VLS_asFunctionalComponent(__VLS_403, new __VLS_403(__assign({ key: (index), value: (user.userId) }, { style: {} })));
        var __VLS_405 = __VLS_404.apply(void 0, __spreadArray([__assign({ key: (index), value: (user.userId) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_404), false));
        __VLS_406.slots.default;
        (user.username);
        var __VLS_406;
    }
    var __VLS_398;
}
var __VLS_373;
var __VLS_407 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_408 = __VLS_asFunctionalComponent(__VLS_407, new __VLS_407({
    key: "2",
    title: "用户组",
}));
var __VLS_409 = __VLS_408.apply(void 0, __spreadArray([{
        key: "2",
        title: "用户组",
    }], __VLS_functionalComponentArgsRest(__VLS_408), false));
__VLS_410.slots.default;
var __VLS_411 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_412 = __VLS_asFunctionalComponent(__VLS_411, new __VLS_411(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { class: "search" }), { placeholder: "输入用户组名或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchSelectUserGroupKeyword) })));
var __VLS_413 = __VLS_412.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { class: "search" }), { placeholder: "输入用户组名或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchSelectUserGroupKeyword) })], __VLS_functionalComponentArgsRest(__VLS_412), false));
var __VLS_415;
var __VLS_416;
var __VLS_417;
var __VLS_418 = {
    onSearch: (__VLS_ctx.handleSearchUserGroup)
};
var __VLS_419 = {
    onKeyup: (__VLS_ctx.handleSearchUserGroup)
};
var __VLS_420 = {
    onClear: (__VLS_ctx.handleSearchUserGroup)
};
var __VLS_414;
if (__VLS_ctx.allUserGroups.length > 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "check-all-container" }));
    var __VLS_421 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_422 = __VLS_asFunctionalComponent(__VLS_421, new __VLS_421(__assign(__assign({ 'onChange': {} }, { style: {} }), { modelValue: (__VLS_ctx.selectUserGroupCheckAll), indeterminate: (__VLS_ctx.selectUserGroupIndeterminate) })));
    var __VLS_423 = __VLS_422.apply(void 0, __spreadArray([__assign(__assign({ 'onChange': {} }, { style: {} }), { modelValue: (__VLS_ctx.selectUserGroupCheckAll), indeterminate: (__VLS_ctx.selectUserGroupIndeterminate) })], __VLS_functionalComponentArgsRest(__VLS_422), false));
    var __VLS_425 = void 0;
    var __VLS_426 = void 0;
    var __VLS_427 = void 0;
    var __VLS_428 = {
        onChange: (__VLS_ctx.handleChangeSelectUserGroupCheckAll)
    };
    __VLS_424.slots.default;
    var __VLS_424;
}
if (__VLS_ctx.allUserGroups.length === 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "empty-container" }));
    var __VLS_429 = {}.AEmpty;
    /** @type {[typeof __VLS_components.AEmpty, typeof __VLS_components.aEmpty, ]} */ ;
    // @ts-ignore
    var __VLS_430 = __VLS_asFunctionalComponent(__VLS_429, new __VLS_429({}));
    var __VLS_431 = __VLS_430.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_430), false));
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign(__assign({ onScroll: (__VLS_ctx.handleAllUserGroupsContainerScroll) }, { class: "scroll-container" }), { ref: "allUserGroupsContainerRef" }));
    /** @type {typeof __VLS_ctx.allUserGroupsContainerRef} */ ;
    var __VLS_433 = {}.ACheckboxGroup;
    /** @type {[typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, ]} */ ;
    // @ts-ignore
    var __VLS_434 = __VLS_asFunctionalComponent(__VLS_433, new __VLS_433(__assign(__assign(__assign({ 'onChange': {} }, { direction: "vertical" }), { style: {} }), { modelValue: (__VLS_ctx.addRolePrincipalsForm.userGroupIds) })));
    var __VLS_435 = __VLS_434.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onChange': {} }, { direction: "vertical" }), { style: {} }), { modelValue: (__VLS_ctx.addRolePrincipalsForm.userGroupIds) })], __VLS_functionalComponentArgsRest(__VLS_434), false));
    var __VLS_437 = void 0;
    var __VLS_438 = void 0;
    var __VLS_439 = void 0;
    var __VLS_440 = {
        onChange: (__VLS_ctx.handleSelectUserGroupChange)
    };
    __VLS_436.slots.default;
    for (var _f = 0, _g = __VLS_getVForSourceType((__VLS_ctx.allUserGroups)); _f < _g.length; _f++) {
        var _h = _g[_f], userGroup = _h[0], index = _h[1];
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-userGroup-item" }));
        var __VLS_441 = {}.ACheckbox;
        /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
        // @ts-ignore
        var __VLS_442 = __VLS_asFunctionalComponent(__VLS_441, new __VLS_441(__assign({ key: (index), value: (userGroup.id) }, { style: {} })));
        var __VLS_443 = __VLS_442.apply(void 0, __spreadArray([__assign({ key: (index), value: (userGroup.id) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_442), false));
        __VLS_444.slots.default;
        (userGroup.name);
        var __VLS_444;
    }
    var __VLS_436;
}
var __VLS_410;
var __VLS_369;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-principal-result" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-principal-result-title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
(__VLS_ctx.selectedPrincipals.length);
var __VLS_445 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_446 = __VLS_asFunctionalComponent(__VLS_445, new __VLS_445(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
var __VLS_447 = __VLS_446.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_446), false));
var __VLS_449;
var __VLS_450;
var __VLS_451;
var __VLS_452 = {
    onClick: (__VLS_ctx.handleClearSelctedPrincipals)
};
__VLS_448.slots.default;
var __VLS_448;
if (__VLS_ctx.selectedPrincipals.length === 0) {
    var __VLS_453 = {}.AEmpty;
    /** @type {[typeof __VLS_components.AEmpty, typeof __VLS_components.aEmpty, ]} */ ;
    // @ts-ignore
    var __VLS_454 = __VLS_asFunctionalComponent(__VLS_453, new __VLS_453({}));
    var __VLS_455 = __VLS_454.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_454), false));
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "scroll-container" }));
    var _loop_1 = function (principal, index) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "selected-principal-item" }, { key: (index) }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
        (principal.username ? principal.username : principal.name);
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!!(__VLS_ctx.selectedPrincipals.length === 0))
                    return;
                __VLS_ctx.handleRemoveSelectedPrincipal(principal);
            } }, { class: "remove-container" }));
        var __VLS_457 = {}.IconClose;
        /** @type {[typeof __VLS_components.IconClose, typeof __VLS_components.iconClose, ]} */ ;
        // @ts-ignore
        var __VLS_458 = __VLS_asFunctionalComponent(__VLS_457, new __VLS_457({}));
        var __VLS_459 = __VLS_458.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_458), false));
    };
    for (var _j = 0, _k = __VLS_getVForSourceType((__VLS_ctx.selectedPrincipals)); _j < _k.length; _j++) {
        var _l = _k[_j], principal = _l[0], index = _l[1];
        _loop_1(principal, index);
    }
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "operation-container" }));
var __VLS_461 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_462 = __VLS_asFunctionalComponent(__VLS_461, new __VLS_461({}));
var __VLS_463 = __VLS_462.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_462), false));
__VLS_464.slots.default;
var __VLS_465 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_466 = __VLS_asFunctionalComponent(__VLS_465, new __VLS_465(__assign({ 'onClick': {} })));
var __VLS_467 = __VLS_466.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_466), false));
var __VLS_469;
var __VLS_470;
var __VLS_471;
var __VLS_472 = {
    onClick: (__VLS_ctx.handleCloseAddRolePrincipalModel)
};
__VLS_468.slots.default;
var __VLS_468;
var __VLS_473 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_474 = __VLS_asFunctionalComponent(__VLS_473, new __VLS_473(__assign({ 'onClick': {} }, { type: "primary", loading: (__VLS_ctx.addRolePrincipalsFormSubmitLoading) })));
var __VLS_475 = __VLS_474.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary", loading: (__VLS_ctx.addRolePrincipalsFormSubmitLoading) })], __VLS_functionalComponentArgsRest(__VLS_474), false));
var __VLS_477;
var __VLS_478;
var __VLS_479;
var __VLS_480 = {
    onClick: (__VLS_ctx.handleAddRolePrincipalsFormSubmit)
};
__VLS_476.slots.default;
var __VLS_476;
var __VLS_464;
var __VLS_361;
var __VLS_481 = {}.authorize;
/** @type {[typeof __VLS_components.Authorize, typeof __VLS_components.authorize, ]} */ ;
// @ts-ignore
var __VLS_482 = __VLS_asFunctionalComponent(__VLS_481, new __VLS_481(__assign({ 'onClose': {} }, { visible: (__VLS_ctx.authorizeVisible) })));
var __VLS_483 = __VLS_482.apply(void 0, __spreadArray([__assign({ 'onClose': {} }, { visible: (__VLS_ctx.authorizeVisible) })], __VLS_functionalComponentArgsRest(__VLS_482), false));
var __VLS_485;
var __VLS_486;
var __VLS_487;
var __VLS_488 = {
    onClose: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.authorizeVisible = false;
    }
};
var __VLS_484;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['tools-container']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['add-container']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
/** @type {__VLS_StyleScopedClasses['condition-container']} */ ;
/** @type {__VLS_StyleScopedClasses['permission-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['filter-footer']} */ ;
/** @type {__VLS_StyleScopedClasses['permission-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['filter-footer']} */ ;
/** @type {__VLS_StyleScopedClasses['permission-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['filter-footer']} */ ;
/** @type {__VLS_StyleScopedClasses['permission-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['filter-footer']} */ ;
/** @type {__VLS_StyleScopedClasses['select-principal-container']} */ ;
/** @type {__VLS_StyleScopedClasses['select-principal-source']} */ ;
/** @type {__VLS_StyleScopedClasses['search']} */ ;
/** @type {__VLS_StyleScopedClasses['check-all-container']} */ ;
/** @type {__VLS_StyleScopedClasses['empty-container']} */ ;
/** @type {__VLS_StyleScopedClasses['scroll-container']} */ ;
/** @type {__VLS_StyleScopedClasses['select-user-item']} */ ;
/** @type {__VLS_StyleScopedClasses['search']} */ ;
/** @type {__VLS_StyleScopedClasses['check-all-container']} */ ;
/** @type {__VLS_StyleScopedClasses['empty-container']} */ ;
/** @type {__VLS_StyleScopedClasses['scroll-container']} */ ;
/** @type {__VLS_StyleScopedClasses['select-userGroup-item']} */ ;
/** @type {__VLS_StyleScopedClasses['select-principal-result']} */ ;
/** @type {__VLS_StyleScopedClasses['select-principal-result-title']} */ ;
/** @type {__VLS_StyleScopedClasses['scroll-container']} */ ;
/** @type {__VLS_StyleScopedClasses['selected-principal-item']} */ ;
/** @type {__VLS_StyleScopedClasses['remove-container']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-container']} */ ;
// @ts-ignore
var __VLS_33 = __VLS_32;
var __VLS_dollars;
var __VLS_self;
