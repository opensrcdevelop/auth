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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "template-header" }));
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
    onClick: (__VLS_ctx.handleToCreateTemplate)
};
__VLS_3.slots.default;
var __VLS_3;
var __VLS_8 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入模板名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchKeyword) })));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入模板名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.searchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_9), false));
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
        __VLS_ctx.handleGetTemplateList();
    }
};
var __VLS_16 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetTemplateList();
    }
};
var __VLS_17 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetTemplateList();
    }
};
var __VLS_11;
if (__VLS_ctx.templateList.length > 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "template-list" }));
    var _loop_1 = function (item) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!(__VLS_ctx.templateList.length > 0))
                    return;
                __VLS_ctx.handleToTemplateDetail(item.id);
            } }, { class: "template-item" }), { key: (item.id) }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "icon" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.svg, __VLS_intrinsicElements.svg)(__assign(__assign({ t: "1751901319177" }, { class: "icon" }), { viewBox: "0 0 1024 1024", version: "1.1", xmlns: "http://www.w3.org/2000/svg", 'p-id': "10006", width: "64", height: "64" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.path, __VLS_intrinsicElements.path)({
            d: "M358.4 298.666667a85.333333 85.333333 0 0 1 85.333333-85.333334h85.333334a42.666667 42.666667 0 0 0 42.666666-42.666666 42.666667 42.666667 0 0 0-42.666666-42.666667h-85.333334a170.666667 170.666667 0 0 0-170.666666 170.666667v597.333333a42.666667 42.666667 0 0 0 42.666666 42.666667 42.666667 42.666667 0 0 0 42.666667-42.666667z",
            fill: "#fff",
            'p-id': "10007",
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.path, __VLS_intrinsicElements.path)({
            d: "M145.066667 426.666667m42.666666 0l298.666667 0q42.666667 0 42.666667 42.666666l0 0q0 42.666667-42.666667 42.666667l-298.666667 0q-42.666667 0-42.666666-42.666667l0 0q0-42.666667 42.666666-42.666666Z",
            fill: "#fff",
            'p-id': "10008",
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.path, __VLS_intrinsicElements.path)({
            d: "M865.706667 426.666667a42.666667 42.666667 0 0 0-37.12 21.333333l-85.333334 149.333333-85.333333-149.333333a42.666667 42.666667 0 0 0-37.12-21.333333 42.666667 42.666667 0 0 0-36.693333 64l110.933333 192-110.933333 192a42.666667 42.666667 0 0 0 36.693333 64 42.666667 42.666667 0 0 0 37.12-21.333334l85.333333-149.333333 85.333334 149.333333a42.666667 42.666667 0 0 0 37.12 21.333334 42.666667 42.666667 0 0 0 36.693333-64l-110.933333-192 110.933333-192a42.666667 42.666667 0 0 0-36.693333-64z",
            fill: "#fff",
            'p-id': "10009",
        });
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "name" }));
        (item.name);
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "desc" }));
        (item.desc);
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "action" }));
        var __VLS_18 = {}.IconDelete;
        /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
        // @ts-ignore
        var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18(__assign({ 'onClick': {} }, { style: {} })));
        var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_19), false));
        var __VLS_22 = void 0;
        var __VLS_23 = void 0;
        var __VLS_24 = void 0;
        var __VLS_25 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!(__VLS_ctx.templateList.length > 0))
                    return;
                __VLS_ctx.handleDeleteTemplate(item);
            }
        };
    };
    var __VLS_21;
    for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.templateList)); _i < _a.length; _i++) {
        var item = _a[_i][0];
        _loop_1(item);
    }
}
else {
    var __VLS_26 = {}.AEmpty;
    /** @type {[typeof __VLS_components.AEmpty, typeof __VLS_components.aEmpty, ]} */ ;
    // @ts-ignore
    var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26(__assign({ style: {} })));
    var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_27), false));
}
/** @type {__VLS_StyleScopedClasses['template-header']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['template-list']} */ ;
/** @type {__VLS_StyleScopedClasses['template-item']} */ ;
/** @type {__VLS_StyleScopedClasses['icon']} */ ;
/** @type {__VLS_StyleScopedClasses['icon']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['name']} */ ;
/** @type {__VLS_StyleScopedClasses['desc']} */ ;
/** @type {__VLS_StyleScopedClasses['action']} */ ;
var __VLS_dollars;
var __VLS_self;
