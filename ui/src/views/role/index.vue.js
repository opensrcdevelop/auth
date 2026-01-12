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
import roleTs from "./index";
export default roleTs;
debugger; /* PartiallyEnd: #3632/script.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "role-header" }));
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
    onClick: (__VLS_ctx.handleToCreateRole)
};
__VLS_3.slots.default;
var __VLS_3;
var __VLS_8 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入角色名称或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchKeyword) })));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入角色名称或标识进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_9), false));
var __VLS_12;
var __VLS_13;
var __VLS_14;
var __VLS_15 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetRoleList(1, 15);
    }
};
var __VLS_16 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetRoleList(1, 15);
    }
};
var __VLS_17 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetRoleList(1, 15);
    }
};
var __VLS_11;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "role-list" }));
var __VLS_18 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.roleList), bordered: (false), pagination: (__VLS_ctx.roleListPagination.pagination), scroll: ({ y: '100%' }) })));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.roleList), bordered: (false), pagination: (__VLS_ctx.roleListPagination.pagination), scroll: ({ y: '100%' }) })], __VLS_functionalComponentArgsRest(__VLS_19), false));
var __VLS_22;
var __VLS_23;
var __VLS_24;
var __VLS_25 = {
    onPageChange: (__VLS_ctx.roleListPagination.handlePageChange)
};
var __VLS_26 = {
    onPageSizeChange: (__VLS_ctx.roleListPagination.handlePageSizeChange)
};
__VLS_21.slots.default;
{
    var __VLS_thisSlot = __VLS_21.slots.columns;
    var __VLS_27 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_28 = __VLS_asFunctionalComponent(__VLS_27, new __VLS_27({
        title: "角色名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_29 = __VLS_28.apply(void 0, __spreadArray([{
            title: "角色名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_28), false));
    __VLS_30.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_30.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToRoleDetail(record_1);
            } }, { class: "table-column-rolename" }));
        (record_1.name);
    }
    var __VLS_30;
    var __VLS_31 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_32 = __VLS_asFunctionalComponent(__VLS_31, new __VLS_31({
        title: "角色标识",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_33 = __VLS_32.apply(void 0, __spreadArray([{
            title: "角色标识",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_32), false));
    __VLS_34.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_34.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        (record.code);
    }
    var __VLS_34;
    var __VLS_35 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_36 = __VLS_asFunctionalComponent(__VLS_35, new __VLS_35({
        title: "操作",
        width: (60),
    }));
    var __VLS_37 = __VLS_36.apply(void 0, __spreadArray([{
            title: "操作",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_36), false));
    __VLS_38.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_38.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        var __VLS_39 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_40 = __VLS_asFunctionalComponent(__VLS_39, new __VLS_39({}));
        var __VLS_41 = __VLS_40.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_40), false));
        __VLS_42.slots.default;
        var __VLS_43 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_44 = __VLS_asFunctionalComponent(__VLS_43, new __VLS_43({
            type: "text",
        }));
        var __VLS_45 = __VLS_44.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_44), false));
        __VLS_46.slots.default;
        {
            var __VLS_thisSlot_4 = __VLS_46.slots.icon;
            var __VLS_47 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_48 = __VLS_asFunctionalComponent(__VLS_47, new __VLS_47({}));
            var __VLS_49 = __VLS_48.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_48), false));
        }
        var __VLS_46;
        {
            var __VLS_thisSlot_5 = __VLS_42.slots.content;
            var __VLS_51 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_52 = __VLS_asFunctionalComponent(__VLS_51, new __VLS_51(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_53 = __VLS_52.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_52), false));
            var __VLS_55 = void 0;
            var __VLS_56 = void 0;
            var __VLS_57 = void 0;
            var __VLS_58 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleDeleteRole(record_2);
                }
            };
            __VLS_54.slots.default;
            {
                var __VLS_thisSlot_6 = __VLS_54.slots.icon;
                var __VLS_59 = {}.IconDelete;
                /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
                // @ts-ignore
                var __VLS_60 = __VLS_asFunctionalComponent(__VLS_59, new __VLS_59({}));
                var __VLS_61 = __VLS_60.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_60), false));
            }
            var __VLS_54;
        }
        var __VLS_42;
    }
    var __VLS_38;
}
var __VLS_21;
/** @type {__VLS_StyleScopedClasses['role-header']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['role-list']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-rolename']} */ ;
var __VLS_dollars;
var __VLS_self;
