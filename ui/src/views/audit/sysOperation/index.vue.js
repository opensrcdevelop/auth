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
var _a, _b, _c, _d;
import indexTs from "./index";
export default indexTs;
debugger; /* PartiallyEnd: #3632/script.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "log-header" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "left" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "log-filter" }));
var __VLS_0 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({}));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_1), false));
__VLS_3.slots.default;
var __VLS_4 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4(__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { style: ({ width: '240px' }) }), { placeholder: "用户名 / 用户ID / IP", modelValue: (__VLS_ctx.searchKeyword), allowClear: true })));
var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSearch': {} }, { 'onClear': {} }), { style: ({ width: '240px' }) }), { placeholder: "用户名 / 用户ID / IP", modelValue: (__VLS_ctx.searchKeyword), allowClear: true })], __VLS_functionalComponentArgsRest(__VLS_5), false));
var __VLS_8;
var __VLS_9;
var __VLS_10;
var __VLS_11 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetSysOperationLogs();
    }
};
var __VLS_12 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetSysOperationLogs();
    }
};
var __VLS_7;
var __VLS_13 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_14 = __VLS_asFunctionalComponent(__VLS_13, new __VLS_13(__assign(__assign({ 'onChange': {} }, { style: ({ width: '120px' }) }), { placeholder: "操作类型", allowClear: true, modelValue: (__VLS_ctx.operationType) })));
var __VLS_15 = __VLS_14.apply(void 0, __spreadArray([__assign(__assign({ 'onChange': {} }, { style: ({ width: '120px' }) }), { placeholder: "操作类型", allowClear: true, modelValue: (__VLS_ctx.operationType) })], __VLS_functionalComponentArgsRest(__VLS_14), false));
var __VLS_17;
var __VLS_18;
var __VLS_19;
var __VLS_20 = {
    onChange: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetSysOperationLogs();
    }
};
__VLS_16.slots.default;
for (var _i = 0, _e = __VLS_getVForSourceType((__VLS_ctx.operationTypes)); _i < _e.length; _i++) {
    var _f = _e[_i], item = _f[0], index = _f[1];
    var __VLS_21 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_22 = __VLS_asFunctionalComponent(__VLS_21, new __VLS_21({
        key: (index),
        value: (item.value),
    }));
    var __VLS_23 = __VLS_22.apply(void 0, __spreadArray([{
            key: (index),
            value: (item.value),
        }], __VLS_functionalComponentArgsRest(__VLS_22), false));
    __VLS_24.slots.default;
    (item.label);
    var __VLS_24;
}
var __VLS_16;
var __VLS_25 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_26 = __VLS_asFunctionalComponent(__VLS_25, new __VLS_25(__assign(__assign({ 'onChange': {} }, { style: ({ width: '140px' }) }), { placeholder: "资源类型", allowClear: true, modelValue: (__VLS_ctx.resourceType) })));
var __VLS_27 = __VLS_26.apply(void 0, __spreadArray([__assign(__assign({ 'onChange': {} }, { style: ({ width: '140px' }) }), { placeholder: "资源类型", allowClear: true, modelValue: (__VLS_ctx.resourceType) })], __VLS_functionalComponentArgsRest(__VLS_26), false));
var __VLS_29;
var __VLS_30;
var __VLS_31;
var __VLS_32 = {
    onChange: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetSysOperationLogs();
    }
};
__VLS_28.slots.default;
for (var _g = 0, _h = __VLS_getVForSourceType((__VLS_ctx.resourceTypes)); _g < _h.length; _g++) {
    var _j = _h[_g], item = _j[0], index = _j[1];
    var __VLS_33 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_34 = __VLS_asFunctionalComponent(__VLS_33, new __VLS_33({
        key: (index),
        value: (item.id),
    }));
    var __VLS_35 = __VLS_34.apply(void 0, __spreadArray([{
            key: (index),
            value: (item.id),
        }], __VLS_functionalComponentArgsRest(__VLS_34), false));
    __VLS_36.slots.default;
    (item.name);
    var __VLS_36;
}
var __VLS_28;
var __VLS_37 = {}.ATrigger;
/** @type {[typeof __VLS_components.ATrigger, typeof __VLS_components.aTrigger, typeof __VLS_components.ATrigger, typeof __VLS_components.aTrigger, ]} */ ;
// @ts-ignore
var __VLS_38 = __VLS_asFunctionalComponent(__VLS_37, new __VLS_37({
    trigger: "click",
    popupOffset: (8),
}));
var __VLS_39 = __VLS_38.apply(void 0, __spreadArray([{
        trigger: "click",
        popupOffset: (8),
    }], __VLS_functionalComponentArgsRest(__VLS_38), false));
__VLS_40.slots.default;
var __VLS_41 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_42 = __VLS_asFunctionalComponent(__VLS_41, new __VLS_41({}));
var __VLS_43 = __VLS_42.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_42), false));
__VLS_44.slots.default;
{
    var __VLS_thisSlot = __VLS_44.slots.icon;
    var __VLS_45 = {}.IconSettings;
    /** @type {[typeof __VLS_components.IconSettings, typeof __VLS_components.iconSettings, ]} */ ;
    // @ts-ignore
    var __VLS_46 = __VLS_asFunctionalComponent(__VLS_45, new __VLS_45({}));
    var __VLS_47 = __VLS_46.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_46), false));
}
var __VLS_44;
{
    var __VLS_thisSlot = __VLS_40.slots.content;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "column-filter-container" }));
    for (var _k = 0, _l = __VLS_getVForSourceType((__VLS_ctx.columns)); _k < _l.length; _k++) {
        var _m = _l[_k], item = _m[0], index = _m[1];
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "column-filter-item" }, { key: (index) }));
        var __VLS_49 = {}.ACheckbox;
        /** @type {[typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, typeof __VLS_components.ACheckbox, typeof __VLS_components.aCheckbox, ]} */ ;
        // @ts-ignore
        var __VLS_50 = __VLS_asFunctionalComponent(__VLS_49, new __VLS_49(__assign({ style: {} }, { modelValue: (item.visible), disabled: (!item.editable) })));
        var __VLS_51 = __VLS_50.apply(void 0, __spreadArray([__assign({ style: {} }, { modelValue: (item.visible), disabled: (!item.editable) })], __VLS_functionalComponentArgsRest(__VLS_50), false));
        __VLS_52.slots.default;
        (item.label);
        var __VLS_52;
    }
}
var __VLS_40;
var __VLS_53 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_54 = __VLS_asFunctionalComponent(__VLS_53, new __VLS_53(__assign({ 'onClick': {} })));
var __VLS_55 = __VLS_54.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_54), false));
var __VLS_57;
var __VLS_58;
var __VLS_59;
var __VLS_60 = {
    onClick: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetSysOperationLogs();
    }
};
__VLS_56.slots.default;
{
    var __VLS_thisSlot = __VLS_56.slots.icon;
    var __VLS_61 = {}.IconRefresh;
    /** @type {[typeof __VLS_components.IconRefresh, typeof __VLS_components.iconRefresh, ]} */ ;
    // @ts-ignore
    var __VLS_62 = __VLS_asFunctionalComponent(__VLS_61, new __VLS_61({}));
    var __VLS_63 = __VLS_62.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_62), false));
}
var __VLS_56;
var __VLS_3;
var __VLS_65 = {}.ARangePicker;
/** @type {[typeof __VLS_components.ARangePicker, typeof __VLS_components.aRangePicker, ]} */ ;
// @ts-ignore
var __VLS_66 = __VLS_asFunctionalComponent(__VLS_65, new __VLS_65(__assign({ 'onChange': {} }, { position: "br", valueFormat: "YYYY-MM-DD HH:mm:ss", disabledDate: (function (current) { return __VLS_ctx.getDayjs(current).isAfter(new Date()); }), shortcuts: ([
        {
            label: '近 7 天',
            value: function () { return [
                new Date(),
                __VLS_ctx.getDayjs(new Date()).subtract(7, 'day').toDate(),
            ]; },
        },
        {
            label: '近 30 天',
            value: function () { return [
                new Date(),
                __VLS_ctx.getDayjs(new Date()).subtract(30, 'day').toDate(),
            ]; },
        },
        {
            label: '近 90 天',
            value: function () { return [
                new Date(),
                __VLS_ctx.getDayjs(new Date()).subtract(90, 'day').toDate(),
            ]; },
        },
        {
            label: '近 180 天',
            value: function () { return [
                new Date(),
                __VLS_ctx.getDayjs(new Date()).subtract(180, 'day').toDate(),
            ]; },
        },
    ]), modelValue: (__VLS_ctx.dateRange) })));
var __VLS_67 = __VLS_66.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { position: "br", valueFormat: "YYYY-MM-DD HH:mm:ss", disabledDate: (function (current) { return __VLS_ctx.getDayjs(current).isAfter(new Date()); }), shortcuts: ([
            {
                label: '近 7 天',
                value: function () { return [
                    new Date(),
                    __VLS_ctx.getDayjs(new Date()).subtract(7, 'day').toDate(),
                ]; },
            },
            {
                label: '近 30 天',
                value: function () { return [
                    new Date(),
                    __VLS_ctx.getDayjs(new Date()).subtract(30, 'day').toDate(),
                ]; },
            },
            {
                label: '近 90 天',
                value: function () { return [
                    new Date(),
                    __VLS_ctx.getDayjs(new Date()).subtract(90, 'day').toDate(),
                ]; },
            },
            {
                label: '近 180 天',
                value: function () { return [
                    new Date(),
                    __VLS_ctx.getDayjs(new Date()).subtract(180, 'day').toDate(),
                ]; },
            },
        ]), modelValue: (__VLS_ctx.dateRange) })], __VLS_functionalComponentArgsRest(__VLS_66), false));
var __VLS_69;
var __VLS_70;
var __VLS_71;
var __VLS_72 = {
    onChange: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetSysOperationLogs();
    }
};
var __VLS_68;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "log-content" }));
var __VLS_73 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_74 = __VLS_asFunctionalComponent(__VLS_73, new __VLS_73(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.sysOperationLogs), bordered: (false), pagination: (__VLS_ctx.sysOperationLogsPagination.pagination), scroll: ({ y: '100%' }), expandable: ({ width: 30 }), rowKey: "id" })));
var __VLS_75 = __VLS_74.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.sysOperationLogs), bordered: (false), pagination: (__VLS_ctx.sysOperationLogsPagination.pagination), scroll: ({ y: '100%' }), expandable: ({ width: 30 }), rowKey: "id" })], __VLS_functionalComponentArgsRest(__VLS_74), false));
var __VLS_77;
var __VLS_78;
var __VLS_79;
var __VLS_80 = {
    onPageChange: (__VLS_ctx.sysOperationLogsPagination.handlePageChange)
};
var __VLS_81 = {
    onPageSizeChange: (__VLS_ctx.sysOperationLogsPagination.handlePageSizeChange)
};
__VLS_76.slots.default;
{
    var __VLS_thisSlot = __VLS_76.slots.columns;
    var _loop_1 = function (column, index) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            key: (index),
        });
        if (column.visible) {
            var __VLS_82 = {}.ATableColumn;
            /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
            // @ts-ignore
            var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
                title: (column.label),
                width: column.width,
                ellipsis: (column.ellipsis),
            }));
            var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
                    title: (column.label),
                    width: column.width,
                    ellipsis: (column.ellipsis),
                }], __VLS_functionalComponentArgsRest(__VLS_83), false));
            __VLS_85.slots.default;
            {
                var __VLS_thisSlot_1 = __VLS_85.slots.cell;
                var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
                if (column.key === 'username') {
                    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                            var _a = [];
                            for (var _i = 0; _i < arguments.length; _i++) {
                                _a[_i] = arguments[_i];
                            }
                            var $event = _a[0];
                            if (!(column.visible))
                                return;
                            if (!(column.key === 'username'))
                                return;
                            __VLS_ctx.handleToUserDetail(record_1.userId);
                        } }, { class: "link" }));
                    (record_1[column.key]);
                }
                else if (column.key === 'detail') {
                    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
                    __VLS_asFunctionalDirective(__VLS_directives.vHtml)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (record_1[column.key]) }), null, null);
                }
                else if (column.key === 'type') {
                    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
                    ((_a = __VLS_ctx.operationTypes.find(function (item) { return item.value === record_1[column.key]; })) === null || _a === void 0 ? void 0 : _a.label);
                }
                else if (column.key === 'resourceId') {
                    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                            var _a = [];
                            for (var _i = 0; _i < arguments.length; _i++) {
                                _a[_i] = arguments[_i];
                            }
                            var $event = _a[0];
                            if (!(column.visible))
                                return;
                            if (!!(column.key === 'username'))
                                return;
                            if (!!(column.key === 'detail'))
                                return;
                            if (!!(column.key === 'type'))
                                return;
                            if (!(column.key === 'resourceId'))
                                return;
                            __VLS_ctx.handleToResourceDetail(record_1.resourceId);
                        } }, { class: "link" }));
                    ((_b = __VLS_ctx.resourceTypes.find(function (item) { return item.id === record_1[column.key]; })) === null || _b === void 0 ? void 0 : _b.name);
                }
                else if (column.key === 'result') {
                    var __VLS_86 = {}.ATag;
                    /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
                    // @ts-ignore
                    var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
                        color: (record_1[column.key] ? '#00b42a' : '#f53f3f'),
                    }));
                    var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
                            color: (record_1[column.key] ? '#00b42a' : '#f53f3f'),
                        }], __VLS_functionalComponentArgsRest(__VLS_87), false));
                    __VLS_89.slots.default;
                    (record_1[column.key] ? "成功" : "失败");
                }
                else {
                    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
                    (record_1[column.key]);
                }
            }
        }
    };
    var __VLS_89, __VLS_85;
    for (var _o = 0, _p = __VLS_getVForSourceType((__VLS_ctx.columns)); _o < _p.length; _o++) {
        var _q = _p[_o], column = _q[0], index = _q[1];
        _loop_1(column, index);
    }
}
{
    var __VLS_thisSlot = __VLS_76.slots["expand-row"];
    var record_2 = __VLS_getSlotParams(__VLS_thisSlot)[0].record;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "all-info-container" }));
    var __VLS_90 = {}.ADescriptions;
    /** @type {[typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, ]} */ ;
    // @ts-ignore
    var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
        column: (3),
    }));
    var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
            column: (3),
        }], __VLS_functionalComponentArgsRest(__VLS_91), false));
    __VLS_93.slots.default;
    var __VLS_94 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
        label: "时间",
    }));
    var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
            label: "时间",
        }], __VLS_functionalComponentArgsRest(__VLS_95), false));
    __VLS_97.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (record_2.time);
    var __VLS_97;
    var __VLS_98 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
        label: "操作类型",
    }));
    var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
            label: "操作类型",
        }], __VLS_functionalComponentArgsRest(__VLS_99), false));
    __VLS_101.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    ((_c = __VLS_ctx.operationTypes.find(function (item) { return item.value === record_2.type; })) === null || _c === void 0 ? void 0 : _c.label);
    var __VLS_101;
    var __VLS_102 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
        label: "资源类型",
    }));
    var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
            label: "资源类型",
        }], __VLS_functionalComponentArgsRest(__VLS_103), false));
    __VLS_105.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleToResourceDetail(record_2.resourceId);
        } }, { class: "link" }));
    ((_d = __VLS_ctx.resourceTypes.find(function (item) { return item.id === record_2.resourceId; })) === null || _d === void 0 ? void 0 : _d.name);
    var __VLS_105;
    var __VLS_106 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({
        span: (3),
    }));
    var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{
            span: (3),
        }], __VLS_functionalComponentArgsRest(__VLS_107), false));
    __VLS_109.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_109.slots.label;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        if (record_2.type == 2 && record_2.result) {
            var __VLS_110 = {}.AButton;
            /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
            // @ts-ignore
            var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110(__assign(__assign({ 'onClick': {} }, { type: "text", size: "mini" }), { style: {} })));
            var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { type: "text", size: "mini" }), { style: {} })], __VLS_functionalComponentArgsRest(__VLS_111), false));
            var __VLS_114 = void 0;
            var __VLS_115 = void 0;
            var __VLS_116 = void 0;
            var __VLS_117 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    if (!(record_2.type == 2 && record_2.result))
                        return;
                    __VLS_ctx.handleGetObjChanges(record_2.id);
                }
            };
            __VLS_113.slots.default;
            var __VLS_113;
        }
    }
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div)({});
    __VLS_asFunctionalDirective(__VLS_directives.vHtml)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (record_2.detail) }), null, null);
    var __VLS_109;
    var __VLS_118 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
        label: "用户名",
    }));
    var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
            label: "用户名",
        }], __VLS_functionalComponentArgsRest(__VLS_119), false));
    __VLS_121.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (record_2.username);
    var __VLS_121;
    var __VLS_122 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
        label: "用户 ID",
        span: (2),
    }));
    var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
            label: "用户 ID",
            span: (2),
        }], __VLS_functionalComponentArgsRest(__VLS_123), false));
    __VLS_125.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (record_2.userId);
    var __VLS_125;
    var __VLS_126 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126({
        label: "请求 ID",
    }));
    var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([{
            label: "请求 ID",
        }], __VLS_functionalComponentArgsRest(__VLS_127), false));
    __VLS_129.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (record_2.requestId);
    var __VLS_129;
    var __VLS_130 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130({
        label: "IP",
    }));
    var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{
            label: "IP",
        }], __VLS_functionalComponentArgsRest(__VLS_131), false));
    __VLS_133.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (record_2.ip);
    var __VLS_133;
    var __VLS_134 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134({
        label: "IP 归属地",
    }));
    var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([{
            label: "IP 归属地",
        }], __VLS_functionalComponentArgsRest(__VLS_135), false));
    __VLS_137.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (record_2.ipRegion);
    var __VLS_137;
    var __VLS_138 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_139 = __VLS_asFunctionalComponent(__VLS_138, new __VLS_138({
        label: "设备类型",
    }));
    var __VLS_140 = __VLS_139.apply(void 0, __spreadArray([{
            label: "设备类型",
        }], __VLS_functionalComponentArgsRest(__VLS_139), false));
    __VLS_141.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (record_2.deviceType);
    var __VLS_141;
    var __VLS_142 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_143 = __VLS_asFunctionalComponent(__VLS_142, new __VLS_142({
        label: "OS 类型",
    }));
    var __VLS_144 = __VLS_143.apply(void 0, __spreadArray([{
            label: "OS 类型",
        }], __VLS_functionalComponentArgsRest(__VLS_143), false));
    __VLS_145.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (record_2.osType);
    var __VLS_145;
    var __VLS_146 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_147 = __VLS_asFunctionalComponent(__VLS_146, new __VLS_146({
        label: "浏览器类型",
    }));
    var __VLS_148 = __VLS_147.apply(void 0, __spreadArray([{
            label: "浏览器类型",
        }], __VLS_functionalComponentArgsRest(__VLS_147), false));
    __VLS_149.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
    (record_2.browserType);
    var __VLS_149;
    var __VLS_150 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_151 = __VLS_asFunctionalComponent(__VLS_150, new __VLS_150({
        label: "额外信息",
        span: (3),
    }));
    var __VLS_152 = __VLS_151.apply(void 0, __spreadArray([{
            label: "额外信息",
            span: (3),
        }], __VLS_functionalComponentArgsRest(__VLS_151), false));
    __VLS_153.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "pre-wrap" }));
    (record_2.extraInfo);
    var __VLS_153;
    var __VLS_93;
}
{
    var __VLS_thisSlot = __VLS_76.slots["expand-icon"];
    var expanded = __VLS_getSlotParams(__VLS_thisSlot)[0].expanded;
    if (!expanded) {
        var __VLS_154 = {}.IconRight;
        /** @type {[typeof __VLS_components.IconRight, typeof __VLS_components.iconRight, ]} */ ;
        // @ts-ignore
        var __VLS_155 = __VLS_asFunctionalComponent(__VLS_154, new __VLS_154({}));
        var __VLS_156 = __VLS_155.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_155), false));
    }
    if (expanded) {
        var __VLS_158 = {}.IconDown;
        /** @type {[typeof __VLS_components.IconDown, typeof __VLS_components.iconDown, ]} */ ;
        // @ts-ignore
        var __VLS_159 = __VLS_asFunctionalComponent(__VLS_158, new __VLS_158({}));
        var __VLS_160 = __VLS_159.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_159), false));
    }
}
var __VLS_76;
var __VLS_162 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_163 = __VLS_asFunctionalComponent(__VLS_162, new __VLS_162(__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.objChangesModalVisible), footer: (false), draggable: true, mask: (false), width: (680), modalStyle: ({
        border: '1px solid var(--color-neutral-3)'
    }) })));
var __VLS_164 = __VLS_163.apply(void 0, __spreadArray([__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.objChangesModalVisible), footer: (false), draggable: true, mask: (false), width: (680), modalStyle: ({
            border: '1px solid var(--color-neutral-3)'
        }) })], __VLS_functionalComponentArgsRest(__VLS_163), false));
var __VLS_166;
var __VLS_167;
var __VLS_168;
var __VLS_169 = {
    onCancel: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.objChangesModalVisible = false;
    }
};
__VLS_165.slots.default;
{
    var __VLS_thisSlot = __VLS_165.slots.title;
}
var __VLS_170 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_171 = __VLS_asFunctionalComponent(__VLS_170, new __VLS_170({
    data: (__VLS_ctx.objChanges),
    pagination: (false),
}));
var __VLS_172 = __VLS_171.apply(void 0, __spreadArray([{
        data: (__VLS_ctx.objChanges),
        pagination: (false),
    }], __VLS_functionalComponentArgsRest(__VLS_171), false));
__VLS_173.slots.default;
{
    var __VLS_thisSlot = __VLS_173.slots.columns;
    var __VLS_174 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_175 = __VLS_asFunctionalComponent(__VLS_174, new __VLS_174({
        title: "No",
    }));
    var __VLS_176 = __VLS_175.apply(void 0, __spreadArray([{
            title: "No",
        }], __VLS_functionalComponentArgsRest(__VLS_175), false));
    __VLS_177.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_177.slots.cell;
        var rowIndex = __VLS_getSlotParams(__VLS_thisSlot_3)[0].rowIndex;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (rowIndex + 1);
    }
    var __VLS_177;
    var __VLS_178 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_179 = __VLS_asFunctionalComponent(__VLS_178, new __VLS_178({
        title: "变更",
    }));
    var __VLS_180 = __VLS_179.apply(void 0, __spreadArray([{
            title: "变更",
        }], __VLS_functionalComponentArgsRest(__VLS_179), false));
    __VLS_181.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_181.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "pre-wrap" }));
        (record);
    }
    var __VLS_181;
}
var __VLS_173;
var __VLS_165;
/** @type {__VLS_StyleScopedClasses['log-header']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['log-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['column-filter-container']} */ ;
/** @type {__VLS_StyleScopedClasses['column-filter-item']} */ ;
/** @type {__VLS_StyleScopedClasses['log-content']} */ ;
/** @type {__VLS_StyleScopedClasses['link']} */ ;
/** @type {__VLS_StyleScopedClasses['link']} */ ;
/** @type {__VLS_StyleScopedClasses['all-info-container']} */ ;
/** @type {__VLS_StyleScopedClasses['link']} */ ;
/** @type {__VLS_StyleScopedClasses['pre-wrap']} */ ;
/** @type {__VLS_StyleScopedClasses['pre-wrap']} */ ;
var __VLS_dollars;
var __VLS_self;
