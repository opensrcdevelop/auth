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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "setting-header" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "left" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
var __VLS_0 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onClick: (__VLS_ctx.handleToCreatePasswordPolicy)
};
__VLS_3.slots.default;
var __VLS_3;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
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
    key: "password_policy",
    title: "密码策略",
}));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([{
        key: "password_policy",
        title: "密码策略",
    }], __VLS_functionalComponentArgsRest(__VLS_17), false));
__VLS_19.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
var __VLS_20 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20(__assign({ 'onChange': {} }, { data: (__VLS_ctx.passwordPolicyList), bordered: (false), scroll: ({ y: '100%' }), pagination: (false), draggable: ({ type: 'handle', width: 20 }) })));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { data: (__VLS_ctx.passwordPolicyList), bordered: (false), scroll: ({ y: '100%' }), pagination: (false), draggable: ({ type: 'handle', width: 20 }) })], __VLS_functionalComponentArgsRest(__VLS_21), false));
var __VLS_24;
var __VLS_25;
var __VLS_26;
var __VLS_27 = {
    onChange: (__VLS_ctx.handlTableChange)
};
__VLS_23.slots.default;
{
    var __VLS_thisSlot = __VLS_23.slots.columns;
    var __VLS_28 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
        title: "执行顺序",
        width: (100),
    }));
    var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
            title: "执行顺序",
            width: (100),
        }], __VLS_functionalComponentArgsRest(__VLS_29), false));
    __VLS_31.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_31.slots.cell;
        var rowIndex = __VLS_getSlotParams(__VLS_thisSlot_1)[0].rowIndex;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (rowIndex + 1);
    }
    var __VLS_31;
    var __VLS_32 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
        title: "策略名称",
        ellipsis: true,
        tooltip: true,
    }));
    var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([{
            title: "策略名称",
            ellipsis: true,
            tooltip: true,
        }], __VLS_functionalComponentArgsRest(__VLS_33), false));
    __VLS_35.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_35.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToPasswordPolicyDetail(record_1.id);
            } }, { class: "table-column-policyname" }));
        (record_1.name);
    }
    var __VLS_35;
    var __VLS_36 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
        title: "密码强度",
    }));
    var __VLS_38 = __VLS_37.apply(void 0, __spreadArray([{
            title: "密码强度",
        }], __VLS_functionalComponentArgsRest(__VLS_37), false));
    __VLS_39.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_39.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        var __VLS_40 = {}.ATag;
        /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
        // @ts-ignore
        var __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
            color: "gray",
        }));
        var __VLS_42 = __VLS_41.apply(void 0, __spreadArray([{
                color: "gray",
            }], __VLS_functionalComponentArgsRest(__VLS_41), false));
        __VLS_43.slots.default;
        (__VLS_ctx.getPasswordStrengthLabel(record.passwordStrength));
        var __VLS_43;
    }
    var __VLS_39;
    var __VLS_44 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
        title: "状态",
    }));
    var __VLS_46 = __VLS_45.apply(void 0, __spreadArray([{
            title: "状态",
        }], __VLS_functionalComponentArgsRest(__VLS_45), false));
    __VLS_47.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_47.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        var __VLS_48 = {}.ASwitch;
        /** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
        // @ts-ignore
        var __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48(__assign({ 'onChange': {} }, { modelValue: (record_2.enabled), type: "round", size: "small" })));
        var __VLS_50 = __VLS_49.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { modelValue: (record_2.enabled), type: "round", size: "small" })], __VLS_functionalComponentArgsRest(__VLS_49), false));
        var __VLS_52 = void 0;
        var __VLS_53 = void 0;
        var __VLS_54 = void 0;
        var __VLS_55 = {
            onChange: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleUpdatePasswordPolicyState(record_2);
            }
        };
        var __VLS_51;
    }
    var __VLS_47;
    var __VLS_56 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
        title: "操作",
        width: (60),
    }));
    var __VLS_58 = __VLS_57.apply(void 0, __spreadArray([{
            title: "操作",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_57), false));
    __VLS_59.slots.default;
    {
        var __VLS_thisSlot_5 = __VLS_59.slots.cell;
        var record_3 = __VLS_getSlotParams(__VLS_thisSlot_5)[0].record;
        var __VLS_60 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60({}));
        var __VLS_62 = __VLS_61.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_61), false));
        __VLS_63.slots.default;
        var __VLS_64 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_65 = __VLS_asFunctionalComponent(__VLS_64, new __VLS_64({
            type: "text",
        }));
        var __VLS_66 = __VLS_65.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_65), false));
        __VLS_67.slots.default;
        {
            var __VLS_thisSlot_6 = __VLS_67.slots.icon;
            var __VLS_68 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({}));
            var __VLS_70 = __VLS_69.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_69), false));
        }
        var __VLS_67;
        {
            var __VLS_thisSlot_7 = __VLS_63.slots.content;
            var __VLS_72 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_74 = __VLS_73.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_73), false));
            var __VLS_76 = void 0;
            var __VLS_77 = void 0;
            var __VLS_78 = void 0;
            var __VLS_79 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleDeletePasswordPolicy(record_3);
                }
            };
            __VLS_75.slots.default;
            {
                var __VLS_thisSlot_8 = __VLS_75.slots.icon;
                var __VLS_80 = {}.IconDelete;
                /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
                // @ts-ignore
                var __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({}));
                var __VLS_82 = __VLS_81.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_81), false));
            }
            var __VLS_75;
        }
        var __VLS_63;
    }
    var __VLS_59;
}
var __VLS_23;
var __VLS_19;
var __VLS_84 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
    key: "remind_logs",
    title: "密码到期提醒记录",
}));
var __VLS_86 = __VLS_85.apply(void 0, __spreadArray([{
        key: "remind_logs",
        title: "密码到期提醒记录",
    }], __VLS_functionalComponentArgsRest(__VLS_85), false));
__VLS_87.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
var __VLS_88 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px', marginBottom: '16px' }) }), { placeholder: "输入用户名或策略名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.remindLogSearchKeyword) })));
var __VLS_90 = __VLS_89.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px', marginBottom: '16px' }) }), { placeholder: "输入用户名或策略名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.remindLogSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_89), false));
var __VLS_92;
var __VLS_93;
var __VLS_94;
var __VLS_95 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetRemindLogList(1, 15);
    }
};
var __VLS_96 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetRemindLogList(1, 15);
    }
};
var __VLS_97 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetRemindLogList(1, 15);
    }
};
var __VLS_91;
var __VLS_98 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.remindLogList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.remindLogListPagination.pagination) })));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.remindLogList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.remindLogListPagination.pagination) })], __VLS_functionalComponentArgsRest(__VLS_99), false));
var __VLS_102;
var __VLS_103;
var __VLS_104;
var __VLS_105 = {
    onPageChange: (__VLS_ctx.remindLogListPagination.handlePageChange)
};
var __VLS_106 = {
    onPageSizeChange: (__VLS_ctx.remindLogListPagination.handlePageSizeChange)
};
__VLS_101.slots.default;
{
    var __VLS_thisSlot = __VLS_101.slots.columns;
    var __VLS_107 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_108 = __VLS_asFunctionalComponent(__VLS_107, new __VLS_107({
        title: "用户",
    }));
    var __VLS_109 = __VLS_108.apply(void 0, __spreadArray([{
            title: "用户",
        }], __VLS_functionalComponentArgsRest(__VLS_108), false));
    __VLS_110.slots.default;
    {
        var __VLS_thisSlot_9 = __VLS_110.slots.cell;
        var record_4 = __VLS_getSlotParams(__VLS_thisSlot_9)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToUserDetail(record_4.userId);
            } }, { class: "table-column-username" }));
        (record_4.username);
    }
    var __VLS_110;
    var __VLS_111 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_112 = __VLS_asFunctionalComponent(__VLS_111, new __VLS_111({
        title: "密码策略",
    }));
    var __VLS_113 = __VLS_112.apply(void 0, __spreadArray([{
            title: "密码策略",
        }], __VLS_functionalComponentArgsRest(__VLS_112), false));
    __VLS_114.slots.default;
    {
        var __VLS_thisSlot_10 = __VLS_114.slots.cell;
        var record_5 = __VLS_getSlotParams(__VLS_thisSlot_10)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToPasswordPolicyDetail(record_5.policyId);
            } }, { class: "table-column-policyname" }));
        (record_5.policyName);
    }
    var __VLS_114;
    var __VLS_115 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_116 = __VLS_asFunctionalComponent(__VLS_115, new __VLS_115({
        title: "提醒时间",
    }));
    var __VLS_117 = __VLS_116.apply(void 0, __spreadArray([{
            title: "提醒时间",
        }], __VLS_functionalComponentArgsRest(__VLS_116), false));
    __VLS_118.slots.default;
    {
        var __VLS_thisSlot_11 = __VLS_118.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_11)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.remindTime);
    }
    var __VLS_118;
    var __VLS_119 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_120 = __VLS_asFunctionalComponent(__VLS_119, new __VLS_119({
        title: "提醒方式",
    }));
    var __VLS_121 = __VLS_120.apply(void 0, __spreadArray([{
            title: "提醒方式",
        }], __VLS_functionalComponentArgsRest(__VLS_120), false));
    __VLS_122.slots.default;
    {
        var __VLS_thisSlot_12 = __VLS_122.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_12)[0].record;
        if (record.remindMethod === 'MAIL') {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        }
    }
    var __VLS_122;
    var __VLS_123 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_124 = __VLS_asFunctionalComponent(__VLS_123, new __VLS_123({
        title: "结果",
    }));
    var __VLS_125 = __VLS_124.apply(void 0, __spreadArray([{
            title: "结果",
        }], __VLS_functionalComponentArgsRest(__VLS_124), false));
    __VLS_126.slots.default;
    {
        var __VLS_thisSlot_13 = __VLS_126.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_13)[0].record;
        if (record.success) {
            var __VLS_127 = {}.ATag;
            /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
            // @ts-ignore
            var __VLS_128 = __VLS_asFunctionalComponent(__VLS_127, new __VLS_127({
                color: "green",
            }));
            var __VLS_129 = __VLS_128.apply(void 0, __spreadArray([{
                    color: "green",
                }], __VLS_functionalComponentArgsRest(__VLS_128), false));
            __VLS_130.slots.default;
            var __VLS_130;
        }
        else {
            var __VLS_131 = {}.ATag;
            /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
            // @ts-ignore
            var __VLS_132 = __VLS_asFunctionalComponent(__VLS_131, new __VLS_131({
                color: "red",
            }));
            var __VLS_133 = __VLS_132.apply(void 0, __spreadArray([{
                    color: "red",
                }], __VLS_functionalComponentArgsRest(__VLS_132), false));
            __VLS_134.slots.default;
            var __VLS_134;
        }
    }
    var __VLS_126;
}
var __VLS_101;
var __VLS_87;
var __VLS_11;
/** @type {__VLS_StyleScopedClasses['setting-header']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-policyname']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-username']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-policyname']} */ ;
var __VLS_dollars;
var __VLS_self;
