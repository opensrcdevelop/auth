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
import resourceTs from "./index";
export default resourceTs;
debugger; /* PartiallyEnd: #3632/script.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "resource-header" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "left" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
var __VLS_0 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({}));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_1), false));
__VLS_3.slots.default;
var __VLS_4 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4(__assign({ 'onClick': {} })));
var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_5), false));
var __VLS_8;
var __VLS_9;
var __VLS_10;
var __VLS_11 = {
    onClick: (__VLS_ctx.handleAuthorize)
};
__VLS_7.slots.default;
var __VLS_7;
var __VLS_12 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_13), false));
var __VLS_16;
var __VLS_17;
var __VLS_18;
var __VLS_19 = {
    onClick: (__VLS_ctx.handleToCreateResource)
};
__VLS_15.slots.default;
var __VLS_15;
var __VLS_3;
var __VLS_20 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入资源名或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.resourceSearchKeyword) })));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入资源名或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.resourceSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_21), false));
var __VLS_24;
var __VLS_25;
var __VLS_26;
var __VLS_27 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetResourceList(1, 15);
    }
};
var __VLS_28 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetResourceList(1, 15);
    }
};
var __VLS_29 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetResourceList(1, 15);
    }
};
var __VLS_23;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "resource-list" }));
var __VLS_30 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.resourceList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.resourceListPagination.pagination) })));
var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.resourceList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.resourceListPagination.pagination) })], __VLS_functionalComponentArgsRest(__VLS_31), false));
var __VLS_34;
var __VLS_35;
var __VLS_36;
var __VLS_37 = {
    onPageChange: (__VLS_ctx.resourceListPagination.handlePageChange)
};
var __VLS_38 = {
    onPageSizeChange: (__VLS_ctx.resourceListPagination.handlePageSizeChange)
};
__VLS_33.slots.default;
{
    var __VLS_thisSlot = __VLS_33.slots.columns;
    var __VLS_39 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_40 = __VLS_asFunctionalComponent(__VLS_39, new __VLS_39({
        title: "资源名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_41 = __VLS_40.apply(void 0, __spreadArray([{
            title: "资源名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_40), false));
    __VLS_42.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_42.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToResourceDetail(record_1);
            } }, { class: "table-column-resourcename" }));
        (record_1.name);
    }
    var __VLS_42;
    var __VLS_43 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_44 = __VLS_asFunctionalComponent(__VLS_43, new __VLS_43({
        title: "资源标识",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_45 = __VLS_44.apply(void 0, __spreadArray([{
            title: "资源标识",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_44), false));
    __VLS_46.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_46.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.code);
    }
    var __VLS_46;
    var __VLS_47 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_48 = __VLS_asFunctionalComponent(__VLS_47, new __VLS_47({
        title: "资源路径",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_49 = __VLS_48.apply(void 0, __spreadArray([{
            title: "资源路径",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_48), false));
    __VLS_50.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_50.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.api);
    }
    var __VLS_50;
    var __VLS_51 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_52 = __VLS_asFunctionalComponent(__VLS_51, new __VLS_51({
        title: "资源组名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_53 = __VLS_52.apply(void 0, __spreadArray([{
            title: "资源组名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_52), false));
    __VLS_54.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_54.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.resourceGroup.name);
    }
    var __VLS_54;
    var __VLS_55 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_56 = __VLS_asFunctionalComponent(__VLS_55, new __VLS_55({
        title: "操作",
        width: (60),
    }));
    var __VLS_57 = __VLS_56.apply(void 0, __spreadArray([{
            title: "操作",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_56), false));
    __VLS_58.slots.default;
    {
        var __VLS_thisSlot_5 = __VLS_58.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_5)[0].record;
        var __VLS_59 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_60 = __VLS_asFunctionalComponent(__VLS_59, new __VLS_59({}));
        var __VLS_61 = __VLS_60.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_60), false));
        __VLS_62.slots.default;
        var __VLS_63 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_64 = __VLS_asFunctionalComponent(__VLS_63, new __VLS_63({
            type: "text",
        }));
        var __VLS_65 = __VLS_64.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_64), false));
        __VLS_66.slots.default;
        {
            var __VLS_thisSlot_6 = __VLS_66.slots.icon;
            var __VLS_67 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_68 = __VLS_asFunctionalComponent(__VLS_67, new __VLS_67({}));
            var __VLS_69 = __VLS_68.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_68), false));
        }
        var __VLS_66;
        {
            var __VLS_thisSlot_7 = __VLS_62.slots.content;
            var __VLS_71 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_72 = __VLS_asFunctionalComponent(__VLS_71, new __VLS_71(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_73 = __VLS_72.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_72), false));
            var __VLS_75 = void 0;
            var __VLS_76 = void 0;
            var __VLS_77 = void 0;
            var __VLS_78 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleDeleteResource(record_2);
                }
            };
            __VLS_74.slots.default;
            {
                var __VLS_thisSlot_8 = __VLS_74.slots.icon;
                var __VLS_79 = {}.IconDelete;
                /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
                // @ts-ignore
                var __VLS_80 = __VLS_asFunctionalComponent(__VLS_79, new __VLS_79({}));
                var __VLS_81 = __VLS_80.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_80), false));
            }
            var __VLS_74;
        }
        var __VLS_62;
    }
    var __VLS_58;
}
var __VLS_33;
var __VLS_83 = {}.authorize;
/** @type {[typeof __VLS_components.Authorize, typeof __VLS_components.authorize, ]} */ ;
// @ts-ignore
var __VLS_84 = __VLS_asFunctionalComponent(__VLS_83, new __VLS_83(__assign({ 'onClose': {} }, { visible: (__VLS_ctx.authorizeVisible) })));
var __VLS_85 = __VLS_84.apply(void 0, __spreadArray([__assign({ 'onClose': {} }, { visible: (__VLS_ctx.authorizeVisible) })], __VLS_functionalComponentArgsRest(__VLS_84), false));
var __VLS_87;
var __VLS_88;
var __VLS_89;
var __VLS_90 = {
    onClose: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.authorizeVisible = false;
    }
};
var __VLS_86;
/** @type {__VLS_StyleScopedClasses['resource-header']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['resource-list']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-resourcename']} */ ;
var __VLS_dollars;
var __VLS_self;
