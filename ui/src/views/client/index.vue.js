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
import clientTs from "./index";
export default clientTs;
debugger; /* PartiallyEnd: #3632/script.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "client-header" }));
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
    onClick: (__VLS_ctx.toCreateClient)
};
__VLS_3.slots.default;
var __VLS_3;
var __VLS_8 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    defaultActiveKey: "1",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        defaultActiveKey: "1",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
__VLS_11.slots.default;
var __VLS_12 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    key: "1",
    title: "客户端列表",
}));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{
        key: "1",
        title: "客户端列表",
    }], __VLS_functionalComponentArgsRest(__VLS_13), false));
__VLS_15.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "client-search" }));
var __VLS_16 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { modelValue: (__VLS_ctx.searchKeyword) }), { style: ({ width: '320px' }) }), { placeholder: "输入客户端名称进行搜索", allowClear: true })));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { modelValue: (__VLS_ctx.searchKeyword) }), { style: ({ width: '320px' }) }), { placeholder: "输入客户端名称进行搜索", allowClear: true })], __VLS_functionalComponentArgsRest(__VLS_17), false));
var __VLS_20;
var __VLS_21;
var __VLS_22;
var __VLS_23 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetClientList(__VLS_ctx.searchKeyword);
    }
};
var __VLS_24 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetClientList(__VLS_ctx.searchKeyword);
    }
};
var __VLS_19;
if (__VLS_ctx.clientList.length > 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "client-list" }));
    var _loop_1 = function (client, index) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "client-card" }, { key: (client.id) }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "client-info" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!(__VLS_ctx.clientList.length > 0))
                    return;
                __VLS_ctx.toClientDetail(client.id);
            } }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "name" }));
        (client.name);
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
        (client.desc);
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "operation" }));
        var __VLS_25 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_26 = __VLS_asFunctionalComponent(__VLS_25, new __VLS_25({
            trigger: "hover",
        }));
        var __VLS_27 = __VLS_26.apply(void 0, __spreadArray([{
                trigger: "hover",
            }], __VLS_functionalComponentArgsRest(__VLS_26), false));
        __VLS_28.slots.default;
        var __VLS_29 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_30 = __VLS_asFunctionalComponent(__VLS_29, new __VLS_29({
            type: "text",
        }));
        var __VLS_31 = __VLS_30.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_30), false));
        __VLS_32.slots.default;
        {
            var __VLS_thisSlot = __VLS_32.slots.icon;
            var __VLS_33 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_34 = __VLS_asFunctionalComponent(__VLS_33, new __VLS_33({}));
            var __VLS_35 = __VLS_34.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_34), false));
        }
        {
            var __VLS_thisSlot = __VLS_28.slots.content;
            var __VLS_37 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_38 = __VLS_asFunctionalComponent(__VLS_37, new __VLS_37({}));
            var __VLS_39 = __VLS_38.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_38), false));
            __VLS_40.slots.default;
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    if (!(__VLS_ctx.clientList.length > 0))
                        return;
                    __VLS_ctx.handleDeleteClient(client);
                } }, { style: {} }));
            var __VLS_41 = {}.IconDelete;
            /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
            // @ts-ignore
            var __VLS_42 = __VLS_asFunctionalComponent(__VLS_41, new __VLS_41({}));
            var __VLS_43 = __VLS_42.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_42), false));
        }
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "client-id" }));
        (client.id);
    };
    var __VLS_32, __VLS_40, __VLS_28;
    for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.clientList)); _i < _a.length; _i++) {
        var _b = _a[_i], client = _b[0], index = _b[1];
        _loop_1(client, index);
    }
}
else {
    var __VLS_45 = {}.AEmpty;
    /** @type {[typeof __VLS_components.AEmpty, typeof __VLS_components.aEmpty, ]} */ ;
    // @ts-ignore
    var __VLS_46 = __VLS_asFunctionalComponent(__VLS_45, new __VLS_45({}));
    var __VLS_47 = __VLS_46.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_46), false));
}
var __VLS_15;
var __VLS_11;
/** @type {__VLS_StyleScopedClasses['client-header']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['client-search']} */ ;
/** @type {__VLS_StyleScopedClasses['client-list']} */ ;
/** @type {__VLS_StyleScopedClasses['client-card']} */ ;
/** @type {__VLS_StyleScopedClasses['client-info']} */ ;
/** @type {__VLS_StyleScopedClasses['name']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['operation']} */ ;
/** @type {__VLS_StyleScopedClasses['client-id']} */ ;
var __VLS_dollars;
var __VLS_self;
