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
(__VLS_ctx.userGroupName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.userGroupId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.userGroupId),
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
    key: "user_group_info",
    title: "用户组信息",
}));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{
        key: "user_group_info",
        title: "用户组信息",
    }], __VLS_functionalComponentArgsRest(__VLS_21), false));
__VLS_23.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_24 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
    model: (__VLS_ctx.userGroupInfoForm),
    layout: "vertical",
    ref: "userGroupInfoFormRef",
    rules: (__VLS_ctx.userGroupInfoFormRules),
}));
var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.userGroupInfoForm),
        layout: "vertical",
        ref: "userGroupInfoFormRef",
        rules: (__VLS_ctx.userGroupInfoFormRules),
    }], __VLS_functionalComponentArgsRest(__VLS_25), false));
/** @type {typeof __VLS_ctx.userGroupInfoFormRef} */ ;
var __VLS_28 = {};
__VLS_27.slots.default;
var __VLS_30 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30({
    gutter: (24),
}));
var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_31), false));
__VLS_33.slots.default;
var __VLS_34 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
    span: (12),
}));
var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_35), false));
__VLS_37.slots.default;
var __VLS_38 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
    field: "name",
    label: "用户组名称",
}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
        field: "name",
        label: "用户组名称",
    }], __VLS_functionalComponentArgsRest(__VLS_39), false));
__VLS_41.slots.default;
var __VLS_42 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    modelValue: (__VLS_ctx.userGroupInfoForm.name),
    placeholder: "请输入用户组名称",
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userGroupInfoForm.name),
        placeholder: "请输入用户组名称",
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
var __VLS_41;
var __VLS_37;
var __VLS_46 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    span: (12),
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
__VLS_49.slots.default;
var __VLS_50 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
    field: "code",
    label: "用户组标识",
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        field: "code",
        label: "用户组标识",
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
__VLS_53.slots.default;
var __VLS_54 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
    modelValue: (__VLS_ctx.userGroupInfoForm.code),
    placeholder: "请输入用户组标识",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userGroupInfoForm.code),
        placeholder: "请输入用户组标识",
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
var __VLS_53;
var __VLS_49;
var __VLS_33;
var __VLS_58 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
    field: "desc",
    label: "用户组描述",
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        field: "desc",
        label: "用户组描述",
    }], __VLS_functionalComponentArgsRest(__VLS_59), false));
__VLS_61.slots.default;
var __VLS_62 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    modelValue: (__VLS_ctx.userGroupInfoForm.desc),
    placeholder: "请输入用户组描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.userGroupInfoForm.desc),
        placeholder: "请输入用户组描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
var __VLS_61;
if (__VLS_ctx.userGroupInfoForm.type === 'DYNAMIC') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
}
if (__VLS_ctx.userGroupInfoForm.type === 'DYNAMIC') {
    var __VLS_66 = {}.UserGroupConditions;
    /** @type {[typeof __VLS_components.UserGroupConditions, ]} */ ;
    // @ts-ignore
    var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
        conditions: (__VLS_ctx.userGroupInfoForm.conditions),
        ref: "userGroupConditionsRef",
    }));
    var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
            conditions: (__VLS_ctx.userGroupInfoForm.conditions),
            ref: "userGroupConditionsRef",
        }], __VLS_functionalComponentArgsRest(__VLS_67), false));
    /** @type {typeof __VLS_ctx.userGroupConditionsRef} */ ;
    var __VLS_70 = {};
    var __VLS_69;
}
var __VLS_72 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
    hideLabel: true,
}));
var __VLS_74 = __VLS_73.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_73), false));
__VLS_75.slots.default;
var __VLS_76 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({}));
var __VLS_78 = __VLS_77.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_77), false));
__VLS_79.slots.default;
var __VLS_80 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_82 = __VLS_81.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_81), false));
var __VLS_84;
var __VLS_85;
var __VLS_86;
var __VLS_87 = {
    onClick: (__VLS_ctx.handleUserGroupInfoFormSubmit)
};
__VLS_83.slots.default;
var __VLS_83;
var __VLS_88 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88(__assign({ 'onClick': {} })));
var __VLS_90 = __VLS_89.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_89), false));
var __VLS_92;
var __VLS_93;
var __VLS_94;
var __VLS_95 = {
    onClick: (__VLS_ctx.handleResetUserGroupInfoForm)
};
__VLS_91.slots.default;
var __VLS_91;
var __VLS_79;
var __VLS_75;
var __VLS_27;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tools-container" }));
var __VLS_96 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onKeyup': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入用户名 / 邮箱 / 手机号进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchGroupUserKeyword) })));
var __VLS_98 = __VLS_97.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { 'onKeyup': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入用户名 / 邮箱 / 手机号进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchGroupUserKeyword) })], __VLS_functionalComponentArgsRest(__VLS_97), false));
var __VLS_100;
var __VLS_101;
var __VLS_102;
var __VLS_103 = {
    onSearch: (__VLS_ctx.handleSearchGroupUser)
};
var __VLS_104 = {
    onClear: (__VLS_ctx.handleSearchGroupUser)
};
var __VLS_105 = {
    onKeyup: (__VLS_ctx.handleSearchGroupUser)
};
var __VLS_99;
if (__VLS_ctx.userGroupInfoForm.type === 'STATIC') {
    var __VLS_106 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106(__assign({ 'onClick': {} }, { type: "text" })));
    var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text" })], __VLS_functionalComponentArgsRest(__VLS_107), false));
    var __VLS_110 = void 0;
    var __VLS_111 = void 0;
    var __VLS_112 = void 0;
    var __VLS_113 = {
        onClick: (__VLS_ctx.handleOpenAddGroupUserModal)
    };
    __VLS_109.slots.default;
    {
        var __VLS_thisSlot = __VLS_109.slots.icon;
        var __VLS_114 = {}.IconPlus;
        /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
        // @ts-ignore
        var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({}));
        var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_115), false));
    }
    var __VLS_109;
}
var __VLS_118 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118(__assign(__assign({ 'onPageSizeChange': {} }, { 'onPageChange': {} }), { data: (__VLS_ctx.groupUsers), pagination: (__VLS_ctx.groupUsersPagination.pagination), bordered: (false), scroll: ({ y: '100%' }) })));
var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([__assign(__assign({ 'onPageSizeChange': {} }, { 'onPageChange': {} }), { data: (__VLS_ctx.groupUsers), pagination: (__VLS_ctx.groupUsersPagination.pagination), bordered: (false), scroll: ({ y: '100%' }) })], __VLS_functionalComponentArgsRest(__VLS_119), false));
var __VLS_122;
var __VLS_123;
var __VLS_124;
var __VLS_125 = {
    onPageSizeChange: (__VLS_ctx.groupUsersPagination.handlePageSizeChange)
};
var __VLS_126 = {
    onPageChange: (__VLS_ctx.groupUsersPagination.handlePageChange)
};
__VLS_121.slots.default;
{
    var __VLS_thisSlot = __VLS_121.slots.columns;
    var __VLS_127 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_128 = __VLS_asFunctionalComponent(__VLS_127, new __VLS_127({
        title: "用户名",
    }));
    var __VLS_129 = __VLS_128.apply(void 0, __spreadArray([{
            title: "用户名",
        }], __VLS_functionalComponentArgsRest(__VLS_128), false));
    __VLS_130.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_130.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToUserDetail(record_1);
            } }, { class: "table-column-name" }));
        (record_1.username);
    }
    var __VLS_130;
    var __VLS_131 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_132 = __VLS_asFunctionalComponent(__VLS_131, new __VLS_131({
        title: "邮箱",
    }));
    var __VLS_133 = __VLS_132.apply(void 0, __spreadArray([{
            title: "邮箱",
        }], __VLS_functionalComponentArgsRest(__VLS_132), false));
    __VLS_134.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_134.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        (record.emailAddress ? record.emailAddress : "-");
    }
    var __VLS_134;
    var __VLS_135 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_136 = __VLS_asFunctionalComponent(__VLS_135, new __VLS_135({
        title: "手机",
    }));
    var __VLS_137 = __VLS_136.apply(void 0, __spreadArray([{
            title: "手机",
        }], __VLS_functionalComponentArgsRest(__VLS_136), false));
    __VLS_138.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_138.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        (record.phoneNumber ? record.phoneNumber : "-");
    }
    var __VLS_138;
    var __VLS_139 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_140 = __VLS_asFunctionalComponent(__VLS_139, new __VLS_139({
        title: "操作",
        width: (60),
    }));
    var __VLS_141 = __VLS_140.apply(void 0, __spreadArray([{
            title: "操作",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_140), false));
    __VLS_142.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_142.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        var __VLS_143 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_144 = __VLS_asFunctionalComponent(__VLS_143, new __VLS_143({}));
        var __VLS_145 = __VLS_144.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_144), false));
        __VLS_146.slots.default;
        var __VLS_147 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_148 = __VLS_asFunctionalComponent(__VLS_147, new __VLS_147({
            type: "text",
        }));
        var __VLS_149 = __VLS_148.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_148), false));
        __VLS_150.slots.default;
        {
            var __VLS_thisSlot_5 = __VLS_150.slots.icon;
            var __VLS_151 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_152 = __VLS_asFunctionalComponent(__VLS_151, new __VLS_151({}));
            var __VLS_153 = __VLS_152.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_152), false));
        }
        var __VLS_150;
        {
            var __VLS_thisSlot_6 = __VLS_146.slots.content;
            var __VLS_155 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_156 = __VLS_asFunctionalComponent(__VLS_155, new __VLS_155(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_157 = __VLS_156.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_156), false));
            var __VLS_159 = void 0;
            var __VLS_160 = void 0;
            var __VLS_161 = void 0;
            var __VLS_162 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleRemoveGroupUser(record_2);
                }
            };
            __VLS_158.slots.default;
            {
                var __VLS_thisSlot_7 = __VLS_158.slots.icon;
                var __VLS_163 = {}.IconUndo;
                /** @type {[typeof __VLS_components.IconUndo, typeof __VLS_components.iconUndo, ]} */ ;
                // @ts-ignore
                var __VLS_164 = __VLS_asFunctionalComponent(__VLS_163, new __VLS_163({}));
                var __VLS_165 = __VLS_164.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_164), false));
            }
            var __VLS_158;
        }
        var __VLS_146;
    }
    var __VLS_142;
}
var __VLS_121;
var __VLS_23;
var __VLS_167 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_168 = __VLS_asFunctionalComponent(__VLS_167, new __VLS_167({
    key: "permission_management",
    title: "权限管理",
}));
var __VLS_169 = __VLS_168.apply(void 0, __spreadArray([{
        key: "permission_management",
        title: "权限管理",
    }], __VLS_functionalComponentArgsRest(__VLS_168), false));
__VLS_170.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "add-container" }));
var __VLS_171 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_172 = __VLS_asFunctionalComponent(__VLS_171, new __VLS_171(__assign({ 'onClick': {} }, { type: "text" })));
var __VLS_173 = __VLS_172.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text" })], __VLS_functionalComponentArgsRest(__VLS_172), false));
var __VLS_175;
var __VLS_176;
var __VLS_177;
var __VLS_178 = {
    onClick: (__VLS_ctx.handleAuthorize)
};
__VLS_174.slots.default;
{
    var __VLS_thisSlot = __VLS_174.slots.icon;
    var __VLS_179 = {}.IconPlus;
    /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
    // @ts-ignore
    var __VLS_180 = __VLS_asFunctionalComponent(__VLS_179, new __VLS_179({}));
    var __VLS_181 = __VLS_180.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_180), false));
}
var __VLS_174;
var __VLS_183 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_184 = __VLS_asFunctionalComponent(__VLS_183, new __VLS_183(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.permissions), bordered: (false), pagination: (__VLS_ctx.permissionsPagination.pagination), expandable: ({ width: 30 }), rowKey: "authorizeId" })));
var __VLS_185 = __VLS_184.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.permissions), bordered: (false), pagination: (__VLS_ctx.permissionsPagination.pagination), expandable: ({ width: 30 }), rowKey: "authorizeId" })], __VLS_functionalComponentArgsRest(__VLS_184), false));
var __VLS_187;
var __VLS_188;
var __VLS_189;
var __VLS_190 = {
    onPageChange: (__VLS_ctx.permissionsPagination.handlePageChange)
};
var __VLS_191 = {
    onPageSizeChange: (__VLS_ctx.permissionsPagination.handlePageSizeChange)
};
__VLS_186.slots.default;
{
    var __VLS_thisSlot = __VLS_186.slots.columns;
    var __VLS_192 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192({
        title: "资源组",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
        filterable: (__VLS_ctx.resourceGroupNameFilter),
    }));
    var __VLS_194 = __VLS_193.apply(void 0, __spreadArray([{
            title: "资源组",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
            filterable: (__VLS_ctx.resourceGroupNameFilter),
        }], __VLS_functionalComponentArgsRest(__VLS_193), false));
    __VLS_195.slots.default;
    {
        var __VLS_thisSlot_8 = __VLS_195.slots.cell;
        var record_3 = __VLS_getSlotParams(__VLS_thisSlot_8)[0].record;
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
    var __VLS_195;
    var __VLS_196 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196({
        title: "资源",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
        filterable: (__VLS_ctx.resourceNameFilter),
    }));
    var __VLS_198 = __VLS_197.apply(void 0, __spreadArray([{
            title: "资源",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
            filterable: (__VLS_ctx.resourceNameFilter),
        }], __VLS_functionalComponentArgsRest(__VLS_197), false));
    __VLS_199.slots.default;
    {
        var __VLS_thisSlot_9 = __VLS_199.slots.cell;
        var record_4 = __VLS_getSlotParams(__VLS_thisSlot_9)[0].record;
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
    var __VLS_199;
    var __VLS_200 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_201 = __VLS_asFunctionalComponent(__VLS_200, new __VLS_200({
        title: "权限名称",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
        filterable: (__VLS_ctx.permissionNameFilter),
    }));
    var __VLS_202 = __VLS_201.apply(void 0, __spreadArray([{
            title: "权限名称",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
            filterable: (__VLS_ctx.permissionNameFilter),
        }], __VLS_functionalComponentArgsRest(__VLS_201), false));
    __VLS_203.slots.default;
    {
        var __VLS_thisSlot_10 = __VLS_203.slots.cell;
        var record_5 = __VLS_getSlotParams(__VLS_thisSlot_10)[0].record;
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
    var __VLS_203;
    var __VLS_204 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204({
        title: "权限标识",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
        filterable: (__VLS_ctx.permissionCodeFilter),
    }));
    var __VLS_206 = __VLS_205.apply(void 0, __spreadArray([{
            title: "权限标识",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
            filterable: (__VLS_ctx.permissionCodeFilter),
        }], __VLS_functionalComponentArgsRest(__VLS_205), false));
    __VLS_207.slots.default;
    {
        var __VLS_thisSlot_11 = __VLS_207.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_11)[0].record;
        (record.permissionCode);
    }
    var __VLS_207;
    var __VLS_208 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
        title: "优先级",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_210 = __VLS_209.apply(void 0, __spreadArray([{
            title: "优先级",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_209), false));
    __VLS_211.slots.default;
    {
        var __VLS_thisSlot_12 = __VLS_211.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_12)[0].record;
        var __VLS_212 = {}.PriorityTag;
        /** @type {[typeof __VLS_components.PriorityTag, typeof __VLS_components.priorityTag, ]} */ ;
        // @ts-ignore
        var __VLS_213 = __VLS_asFunctionalComponent(__VLS_212, new __VLS_212({
            priority: (record.priority),
        }));
        var __VLS_214 = __VLS_213.apply(void 0, __spreadArray([{
                priority: (record.priority),
            }], __VLS_functionalComponentArgsRest(__VLS_213), false));
    }
    var __VLS_211;
    var __VLS_216 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_217 = __VLS_asFunctionalComponent(__VLS_216, new __VLS_216({
        title: "操作",
    }));
    var __VLS_218 = __VLS_217.apply(void 0, __spreadArray([{
            title: "操作",
        }], __VLS_functionalComponentArgsRest(__VLS_217), false));
    __VLS_219.slots.default;
    {
        var __VLS_thisSlot_13 = __VLS_219.slots.cell;
        var record_6 = __VLS_getSlotParams(__VLS_thisSlot_13)[0].record;
        var __VLS_220 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_221 = __VLS_asFunctionalComponent(__VLS_220, new __VLS_220({}));
        var __VLS_222 = __VLS_221.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_221), false));
        __VLS_223.slots.default;
        var __VLS_224 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_225 = __VLS_asFunctionalComponent(__VLS_224, new __VLS_224({
            type: "text",
        }));
        var __VLS_226 = __VLS_225.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_225), false));
        __VLS_227.slots.default;
        {
            var __VLS_thisSlot_14 = __VLS_227.slots.icon;
            var __VLS_228 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_229 = __VLS_asFunctionalComponent(__VLS_228, new __VLS_228({}));
            var __VLS_230 = __VLS_229.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_229), false));
        }
        var __VLS_227;
        {
            var __VLS_thisSlot_15 = __VLS_223.slots.content;
            var __VLS_232 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_233 = __VLS_asFunctionalComponent(__VLS_232, new __VLS_232(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_234 = __VLS_233.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_233), false));
            var __VLS_236 = void 0;
            var __VLS_237 = void 0;
            var __VLS_238 = void 0;
            var __VLS_239 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleCancelAuthorization(record_6);
                }
            };
            __VLS_235.slots.default;
            {
                var __VLS_thisSlot_16 = __VLS_235.slots.icon;
                var __VLS_240 = {}.IconUndo;
                /** @type {[typeof __VLS_components.IconUndo, typeof __VLS_components.iconUndo, ]} */ ;
                // @ts-ignore
                var __VLS_241 = __VLS_asFunctionalComponent(__VLS_240, new __VLS_240({}));
                var __VLS_242 = __VLS_241.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_241), false));
            }
            var __VLS_235;
        }
        var __VLS_223;
    }
    var __VLS_219;
}
{
    var __VLS_thisSlot = __VLS_186.slots["expand-row"];
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
            var __VLS_244 = {}.ADescriptions;
            /** @type {[typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, ]} */ ;
            // @ts-ignore
            var __VLS_245 = __VLS_asFunctionalComponent(__VLS_244, new __VLS_244(__assign({ style: {} }, { key: (index), column: (1), bordered: true })));
            var __VLS_246 = __VLS_245.apply(void 0, __spreadArray([__assign({ style: {} }, { key: (index), column: (1), bordered: true })], __VLS_functionalComponentArgsRest(__VLS_245), false));
            __VLS_247.slots.default;
            var __VLS_248 = {}.ADescriptionsItem;
            /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
            // @ts-ignore
            var __VLS_249 = __VLS_asFunctionalComponent(__VLS_248, new __VLS_248({
                label: "限制条件名称",
            }));
            var __VLS_250 = __VLS_249.apply(void 0, __spreadArray([{
                    label: "限制条件名称",
                }], __VLS_functionalComponentArgsRest(__VLS_249), false));
            __VLS_251.slots.default;
            var __VLS_252 = {}.ALink;
            /** @type {[typeof __VLS_components.ALink, typeof __VLS_components.aLink, typeof __VLS_components.ALink, typeof __VLS_components.aLink, ]} */ ;
            // @ts-ignore
            var __VLS_253 = __VLS_asFunctionalComponent(__VLS_252, new __VLS_252({}));
            var __VLS_254 = __VLS_253.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_253), false));
            __VLS_255.slots.default;
            __VLS_asFunctionalElement(__VLS_intrinsicElements.a, __VLS_intrinsicElements.a)({
                href: ("/ui/permission/expression/detail?id=".concat(condition.id)),
                target: "_blank",
            });
            (condition.name);
            var __VLS_256 = {}.IconLaunch;
            /** @type {[typeof __VLS_components.IconLaunch, typeof __VLS_components.iconLaunch, ]} */ ;
            // @ts-ignore
            var __VLS_257 = __VLS_asFunctionalComponent(__VLS_256, new __VLS_256(__assign({ style: {} })));
            var __VLS_258 = __VLS_257.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_257), false));
            var __VLS_255;
            var __VLS_251;
            var __VLS_260 = {}.ADescriptionsItem;
            /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
            // @ts-ignore
            var __VLS_261 = __VLS_asFunctionalComponent(__VLS_260, new __VLS_260({
                label: "限制条件描述",
            }));
            var __VLS_262 = __VLS_261.apply(void 0, __spreadArray([{
                    label: "限制条件描述",
                }], __VLS_functionalComponentArgsRest(__VLS_261), false));
            __VLS_263.slots.default;
            (condition.desc ? condition.desc : "-");
            var __VLS_263;
            var __VLS_247;
        }
    }
}
{
    var __VLS_thisSlot = __VLS_186.slots["expand-icon"];
    var expanded = __VLS_getSlotParams(__VLS_thisSlot)[0].expanded;
    if (!expanded) {
        var __VLS_264 = {}.IconRight;
        /** @type {[typeof __VLS_components.IconRight, typeof __VLS_components.iconRight, ]} */ ;
        // @ts-ignore
        var __VLS_265 = __VLS_asFunctionalComponent(__VLS_264, new __VLS_264({}));
        var __VLS_266 = __VLS_265.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_265), false));
    }
    if (expanded) {
        var __VLS_268 = {}.IconDown;
        /** @type {[typeof __VLS_components.IconDown, typeof __VLS_components.iconDown, ]} */ ;
        // @ts-ignore
        var __VLS_269 = __VLS_asFunctionalComponent(__VLS_268, new __VLS_268({}));
        var __VLS_270 = __VLS_269.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_269), false));
    }
}
{
    var __VLS_thisSlot = __VLS_186.slots["resource-group-name-filter"];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "permission-filter" }));
    var __VLS_272 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_273 = __VLS_asFunctionalComponent(__VLS_272, new __VLS_272({
        placeholder: "输入资源组名称进行搜索",
        modelValue: (__VLS_ctx.authorizeSearchKeywords.resourceGroupName),
    }));
    var __VLS_274 = __VLS_273.apply(void 0, __spreadArray([{
            placeholder: "输入资源组名称进行搜索",
            modelValue: (__VLS_ctx.authorizeSearchKeywords.resourceGroupName),
        }], __VLS_functionalComponentArgsRest(__VLS_273), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "filter-footer" }));
    var __VLS_276 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_277 = __VLS_asFunctionalComponent(__VLS_276, new __VLS_276({}));
    var __VLS_278 = __VLS_277.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_277), false));
    __VLS_279.slots.default;
    var __VLS_280 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_281 = __VLS_asFunctionalComponent(__VLS_280, new __VLS_280(__assign({ 'onClick': {} }, { type: "primary" })));
    var __VLS_282 = __VLS_281.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_281), false));
    var __VLS_284 = void 0;
    var __VLS_285 = void 0;
    var __VLS_286 = void 0;
    var __VLS_287 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleGetUserGroupPermissions();
        }
    };
    __VLS_283.slots.default;
    var __VLS_283;
    var __VLS_288 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_289 = __VLS_asFunctionalComponent(__VLS_288, new __VLS_288(__assign({ 'onClick': {} })));
    var __VLS_290 = __VLS_289.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_289), false));
    var __VLS_292 = void 0;
    var __VLS_293 = void 0;
    var __VLS_294 = void 0;
    var __VLS_295 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleResetPermissionFilter('resourceGroupName');
        }
    };
    __VLS_291.slots.default;
    var __VLS_291;
    var __VLS_279;
}
{
    var __VLS_thisSlot = __VLS_186.slots["resource-name-filter"];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "permission-filter" }));
    var __VLS_296 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_297 = __VLS_asFunctionalComponent(__VLS_296, new __VLS_296({
        placeholder: "输入资源名称进行搜索",
        modelValue: (__VLS_ctx.authorizeSearchKeywords.resourceName),
    }));
    var __VLS_298 = __VLS_297.apply(void 0, __spreadArray([{
            placeholder: "输入资源名称进行搜索",
            modelValue: (__VLS_ctx.authorizeSearchKeywords.resourceName),
        }], __VLS_functionalComponentArgsRest(__VLS_297), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "filter-footer" }));
    var __VLS_300 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_301 = __VLS_asFunctionalComponent(__VLS_300, new __VLS_300({}));
    var __VLS_302 = __VLS_301.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_301), false));
    __VLS_303.slots.default;
    var __VLS_304 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_305 = __VLS_asFunctionalComponent(__VLS_304, new __VLS_304(__assign({ 'onClick': {} }, { type: "primary" })));
    var __VLS_306 = __VLS_305.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_305), false));
    var __VLS_308 = void 0;
    var __VLS_309 = void 0;
    var __VLS_310 = void 0;
    var __VLS_311 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleGetUserGroupPermissions();
        }
    };
    __VLS_307.slots.default;
    var __VLS_307;
    var __VLS_312 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_313 = __VLS_asFunctionalComponent(__VLS_312, new __VLS_312(__assign({ 'onClick': {} })));
    var __VLS_314 = __VLS_313.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_313), false));
    var __VLS_316 = void 0;
    var __VLS_317 = void 0;
    var __VLS_318 = void 0;
    var __VLS_319 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleResetPermissionFilter('resourceName');
        }
    };
    __VLS_315.slots.default;
    var __VLS_315;
    var __VLS_303;
}
{
    var __VLS_thisSlot = __VLS_186.slots["permission-name-filter"];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "permission-filter" }));
    var __VLS_320 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_321 = __VLS_asFunctionalComponent(__VLS_320, new __VLS_320({
        placeholder: "输入权限名称进行搜索",
        modelValue: (__VLS_ctx.authorizeSearchKeywords.permissionName),
    }));
    var __VLS_322 = __VLS_321.apply(void 0, __spreadArray([{
            placeholder: "输入权限名称进行搜索",
            modelValue: (__VLS_ctx.authorizeSearchKeywords.permissionName),
        }], __VLS_functionalComponentArgsRest(__VLS_321), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "filter-footer" }));
    var __VLS_324 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_325 = __VLS_asFunctionalComponent(__VLS_324, new __VLS_324({}));
    var __VLS_326 = __VLS_325.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_325), false));
    __VLS_327.slots.default;
    var __VLS_328 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_329 = __VLS_asFunctionalComponent(__VLS_328, new __VLS_328(__assign({ 'onClick': {} }, { type: "primary" })));
    var __VLS_330 = __VLS_329.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_329), false));
    var __VLS_332 = void 0;
    var __VLS_333 = void 0;
    var __VLS_334 = void 0;
    var __VLS_335 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleGetUserGroupPermissions();
        }
    };
    __VLS_331.slots.default;
    var __VLS_331;
    var __VLS_336 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_337 = __VLS_asFunctionalComponent(__VLS_336, new __VLS_336(__assign({ 'onClick': {} })));
    var __VLS_338 = __VLS_337.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_337), false));
    var __VLS_340 = void 0;
    var __VLS_341 = void 0;
    var __VLS_342 = void 0;
    var __VLS_343 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleResetPermissionFilter('permissionName');
        }
    };
    __VLS_339.slots.default;
    var __VLS_339;
    var __VLS_327;
}
{
    var __VLS_thisSlot = __VLS_186.slots["permission-code-filter"];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "permission-filter" }));
    var __VLS_344 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_345 = __VLS_asFunctionalComponent(__VLS_344, new __VLS_344({
        placeholder: "输入权限标识进行搜索",
        modelValue: (__VLS_ctx.authorizeSearchKeywords.permissionCode),
    }));
    var __VLS_346 = __VLS_345.apply(void 0, __spreadArray([{
            placeholder: "输入权限标识进行搜索",
            modelValue: (__VLS_ctx.authorizeSearchKeywords.permissionCode),
        }], __VLS_functionalComponentArgsRest(__VLS_345), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "filter-footer" }));
    var __VLS_348 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_349 = __VLS_asFunctionalComponent(__VLS_348, new __VLS_348({}));
    var __VLS_350 = __VLS_349.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_349), false));
    __VLS_351.slots.default;
    var __VLS_352 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_353 = __VLS_asFunctionalComponent(__VLS_352, new __VLS_352(__assign({ 'onClick': {} }, { type: "primary" })));
    var __VLS_354 = __VLS_353.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_353), false));
    var __VLS_356 = void 0;
    var __VLS_357 = void 0;
    var __VLS_358 = void 0;
    var __VLS_359 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleGetUserGroupPermissions();
        }
    };
    __VLS_355.slots.default;
    var __VLS_355;
    var __VLS_360 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_361 = __VLS_asFunctionalComponent(__VLS_360, new __VLS_360(__assign({ 'onClick': {} })));
    var __VLS_362 = __VLS_361.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_361), false));
    var __VLS_364 = void 0;
    var __VLS_365 = void 0;
    var __VLS_366 = void 0;
    var __VLS_367 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleResetPermissionFilter('permissionCode');
        }
    };
    __VLS_363.slots.default;
    var __VLS_363;
    var __VLS_351;
}
var __VLS_186;
var __VLS_170;
var __VLS_15;
var __VLS_3;
var __VLS_368 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_369 = __VLS_asFunctionalComponent(__VLS_368, new __VLS_368(__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.addGroupUserModalVisible), footer: (false), width: (728) })));
var __VLS_370 = __VLS_369.apply(void 0, __spreadArray([__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.addGroupUserModalVisible), footer: (false), width: (728) })], __VLS_functionalComponentArgsRest(__VLS_369), false));
var __VLS_372;
var __VLS_373;
var __VLS_374;
var __VLS_375 = {
    onCancel: (__VLS_ctx.handleCloseAddGroupUserModal)
};
__VLS_371.slots.default;
{
    var __VLS_thisSlot = __VLS_371.slots.title;
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-user-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-user-source" }));
var __VLS_376 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_377 = __VLS_asFunctionalComponent(__VLS_376, new __VLS_376(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { class: "search" }), { placeholder: "输入用户名搜索成员", allowClear: true, modelValue: (__VLS_ctx.searchSelectUserKeyword) })));
var __VLS_378 = __VLS_377.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { class: "search" }), { placeholder: "输入用户名搜索成员", allowClear: true, modelValue: (__VLS_ctx.searchSelectUserKeyword) })], __VLS_functionalComponentArgsRest(__VLS_377), false));
var __VLS_380;
var __VLS_381;
var __VLS_382;
var __VLS_383 = {
    onSearch: (__VLS_ctx.handleSearchUser)
};
var __VLS_384 = {
    onKeyup: (__VLS_ctx.handleSearchUser)
};
var __VLS_385 = {
    onClear: (__VLS_ctx.handleSearchUser)
};
var __VLS_379;
if (__VLS_ctx.allUsers.length > 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "check-all-container" }));
    var __VLS_386 = {}.ACheckbox;
    /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
    // @ts-ignore
    var __VLS_387 = __VLS_asFunctionalComponent(__VLS_386, new __VLS_386(__assign(__assign({ 'onChange': {} }, { style: {} }), { modelValue: (__VLS_ctx.selectUserCheckAll), indeterminate: (__VLS_ctx.selectUserIndeterminate) })));
    var __VLS_388 = __VLS_387.apply(void 0, __spreadArray([__assign(__assign({ 'onChange': {} }, { style: {} }), { modelValue: (__VLS_ctx.selectUserCheckAll), indeterminate: (__VLS_ctx.selectUserIndeterminate) })], __VLS_functionalComponentArgsRest(__VLS_387), false));
    var __VLS_390 = void 0;
    var __VLS_391 = void 0;
    var __VLS_392 = void 0;
    var __VLS_393 = {
        onChange: (__VLS_ctx.handleChangeCheckAll)
    };
    __VLS_389.slots.default;
    var __VLS_389;
}
if (__VLS_ctx.allUsers.length === 0) {
    var __VLS_394 = {}.AEmpty;
    /** @type {[typeof __VLS_components.AEmpty, typeof __VLS_components.aEmpty, ]} */ ;
    // @ts-ignore
    var __VLS_395 = __VLS_asFunctionalComponent(__VLS_394, new __VLS_394({}));
    var __VLS_396 = __VLS_395.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_395), false));
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign(__assign({ onScroll: (__VLS_ctx.handleAllUsersContainerScroll) }, { class: "scroll-container" }), { ref: "allUsersContainerRef" }));
    /** @type {typeof __VLS_ctx.allUsersContainerRef} */ ;
    var __VLS_398 = {}.ACheckboxGroup;
    /** @type {[typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, typeof __VLS_components.ACheckboxGroup, typeof __VLS_components.aCheckboxGroup, ]} */ ;
    // @ts-ignore
    var __VLS_399 = __VLS_asFunctionalComponent(__VLS_398, new __VLS_398(__assign(__assign(__assign({ 'onChange': {} }, { direction: "vertical" }), { style: {} }), { modelValue: (__VLS_ctx.addGroupUsersForm.userIds) })));
    var __VLS_400 = __VLS_399.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onChange': {} }, { direction: "vertical" }), { style: {} }), { modelValue: (__VLS_ctx.addGroupUsersForm.userIds) })], __VLS_functionalComponentArgsRest(__VLS_399), false));
    var __VLS_402 = void 0;
    var __VLS_403 = void 0;
    var __VLS_404 = void 0;
    var __VLS_405 = {
        onChange: (__VLS_ctx.handleSelectUserChange)
    };
    __VLS_401.slots.default;
    for (var _c = 0, _d = __VLS_getVForSourceType((__VLS_ctx.allUsers)); _c < _d.length; _c++) {
        var _e = _d[_c], user = _e[0], index = _e[1];
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-user-item" }));
        var __VLS_406 = {}.ACheckbox;
        /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
        // @ts-ignore
        var __VLS_407 = __VLS_asFunctionalComponent(__VLS_406, new __VLS_406(__assign({ key: (index), value: (user.userId) }, { style: {} })));
        var __VLS_408 = __VLS_407.apply(void 0, __spreadArray([__assign({ key: (index), value: (user.userId) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_407), false));
        __VLS_409.slots.default;
        (user.username);
        var __VLS_409;
    }
    var __VLS_401;
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-user-result" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "select-user-result-title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
(__VLS_ctx.addGroupUsersForm.userIds.length);
var __VLS_410 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_411 = __VLS_asFunctionalComponent(__VLS_410, new __VLS_410(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
var __VLS_412 = __VLS_411.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_411), false));
var __VLS_414;
var __VLS_415;
var __VLS_416;
var __VLS_417 = {
    onClick: (__VLS_ctx.handleClearSelctedUsers)
};
__VLS_413.slots.default;
var __VLS_413;
if (__VLS_ctx.addGroupUsersForm.userIds.length === 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "empty-container" }));
    var __VLS_418 = {}.AEmpty;
    /** @type {[typeof __VLS_components.AEmpty, typeof __VLS_components.aEmpty, ]} */ ;
    // @ts-ignore
    var __VLS_419 = __VLS_asFunctionalComponent(__VLS_418, new __VLS_418({}));
    var __VLS_420 = __VLS_419.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_419), false));
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "scroll-container" }));
    var _loop_1 = function (user, index) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "selected-user-item" }, { key: (index) }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
        (user.username);
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!!(__VLS_ctx.addGroupUsersForm.userIds.length === 0))
                    return;
                __VLS_ctx.handleRemoveSelectedUser(user.userId);
            } }, { class: "remove-container" }));
        var __VLS_422 = {}.IconClose;
        /** @type {[typeof __VLS_components.IconClose, typeof __VLS_components.iconClose, ]} */ ;
        // @ts-ignore
        var __VLS_423 = __VLS_asFunctionalComponent(__VLS_422, new __VLS_422({}));
        var __VLS_424 = __VLS_423.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_423), false));
    };
    for (var _f = 0, _g = __VLS_getVForSourceType((__VLS_ctx.slectedUsers)); _f < _g.length; _f++) {
        var _h = _g[_f], user = _h[0], index = _h[1];
        _loop_1(user, index);
    }
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "operation-container" }));
var __VLS_426 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_427 = __VLS_asFunctionalComponent(__VLS_426, new __VLS_426({}));
var __VLS_428 = __VLS_427.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_427), false));
__VLS_429.slots.default;
var __VLS_430 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_431 = __VLS_asFunctionalComponent(__VLS_430, new __VLS_430(__assign({ 'onClick': {} })));
var __VLS_432 = __VLS_431.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_431), false));
var __VLS_434;
var __VLS_435;
var __VLS_436;
var __VLS_437 = {
    onClick: (__VLS_ctx.handleCloseAddGroupUserModal)
};
__VLS_433.slots.default;
var __VLS_433;
var __VLS_438 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_439 = __VLS_asFunctionalComponent(__VLS_438, new __VLS_438(__assign({ 'onClick': {} }, { type: "primary", loading: (__VLS_ctx.addGroupUsersFormSubmitLoading) })));
var __VLS_440 = __VLS_439.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary", loading: (__VLS_ctx.addGroupUsersFormSubmitLoading) })], __VLS_functionalComponentArgsRest(__VLS_439), false));
var __VLS_442;
var __VLS_443;
var __VLS_444;
var __VLS_445 = {
    onClick: (__VLS_ctx.handleAddGroupUsersFormSubmit)
};
__VLS_441.slots.default;
var __VLS_441;
var __VLS_429;
var __VLS_371;
var __VLS_446 = {}.authorize;
/** @type {[typeof __VLS_components.Authorize, typeof __VLS_components.authorize, ]} */ ;
// @ts-ignore
var __VLS_447 = __VLS_asFunctionalComponent(__VLS_446, new __VLS_446(__assign({ 'onClose': {} }, { visible: (__VLS_ctx.authorizeVisible) })));
var __VLS_448 = __VLS_447.apply(void 0, __spreadArray([__assign({ 'onClose': {} }, { visible: (__VLS_ctx.authorizeVisible) })], __VLS_functionalComponentArgsRest(__VLS_447), false));
var __VLS_450;
var __VLS_451;
var __VLS_452;
var __VLS_453 = {
    onClose: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.authorizeVisible = false;
    }
};
var __VLS_449;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
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
/** @type {__VLS_StyleScopedClasses['select-user-container']} */ ;
/** @type {__VLS_StyleScopedClasses['select-user-source']} */ ;
/** @type {__VLS_StyleScopedClasses['search']} */ ;
/** @type {__VLS_StyleScopedClasses['check-all-container']} */ ;
/** @type {__VLS_StyleScopedClasses['scroll-container']} */ ;
/** @type {__VLS_StyleScopedClasses['select-user-item']} */ ;
/** @type {__VLS_StyleScopedClasses['select-user-result']} */ ;
/** @type {__VLS_StyleScopedClasses['select-user-result-title']} */ ;
/** @type {__VLS_StyleScopedClasses['empty-container']} */ ;
/** @type {__VLS_StyleScopedClasses['scroll-container']} */ ;
/** @type {__VLS_StyleScopedClasses['selected-user-item']} */ ;
/** @type {__VLS_StyleScopedClasses['remove-container']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-container']} */ ;
// @ts-ignore
var __VLS_29 = __VLS_28, __VLS_71 = __VLS_70;
var __VLS_dollars;
var __VLS_self;
