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
var _a;
import { computed } from "vue";
var props = defineProps({
    menu: {
        type: Object,
        required: true,
    }
});
var visible = computed(function () {
    return props.menu.meta.visible();
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
if (((_a = __VLS_ctx.menu.children) === null || _a === void 0 ? void 0 : _a.length) > 0 && __VLS_ctx.visible) {
    var __VLS_0 = {}.ASubMenu;
    /** @type {[typeof __VLS_components.ASubMenu, typeof __VLS_components.aSubMenu, typeof __VLS_components.ASubMenu, typeof __VLS_components.aSubMenu, ]} */ ;
    // @ts-ignore
    var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
        key: (__VLS_ctx.menu.path),
    }));
    var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{
            key: (__VLS_ctx.menu.path),
        }], __VLS_functionalComponentArgsRest(__VLS_1), false));
    __VLS_3.slots.default;
    {
        var __VLS_thisSlot = __VLS_3.slots.icon;
        var __VLS_4 = {}.IconFont;
        /** @type {[typeof __VLS_components.IconFont, typeof __VLS_components.iconFont, typeof __VLS_components.IconFont, typeof __VLS_components.iconFont, ]} */ ;
        // @ts-ignore
        var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4(__assign({ type: (__VLS_ctx.menu.meta.icon) }, { style: {} })));
        var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([__assign({ type: (__VLS_ctx.menu.meta.icon) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_5), false));
    }
    {
        var __VLS_thisSlot = __VLS_3.slots.title;
        (__VLS_ctx.menu.meta.title);
    }
    for (var _i = 0, _b = __VLS_getVForSourceType((__VLS_ctx.menu.children)); _i < _b.length; _i++) {
        var sub = _b[_i][0];
        var __VLS_8 = {}.MenuItem;
        /** @type {[typeof __VLS_components.MenuItem, typeof __VLS_components.menuItem, typeof __VLS_components.MenuItem, typeof __VLS_components.menuItem, ]} */ ;
        // @ts-ignore
        var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
            key: (sub.path),
            menu: (sub),
        }));
        var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
                key: (sub.path),
                menu: (sub),
            }], __VLS_functionalComponentArgsRest(__VLS_9), false));
    }
    var __VLS_3;
}
if (!__VLS_ctx.menu.children && __VLS_ctx.visible) {
    var __VLS_12 = {}.AMenuItem;
    /** @type {[typeof __VLS_components.AMenuItem, typeof __VLS_components.aMenuItem, typeof __VLS_components.AMenuItem, typeof __VLS_components.aMenuItem, ]} */ ;
    // @ts-ignore
    var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign({ 'onClick': {} }, { key: (__VLS_ctx.menu.path) })));
    var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { key: (__VLS_ctx.menu.path) })], __VLS_functionalComponentArgsRest(__VLS_13), false));
    var __VLS_16 = void 0;
    var __VLS_17 = void 0;
    var __VLS_18 = void 0;
    var __VLS_19 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            if (!(!__VLS_ctx.menu.children && __VLS_ctx.visible))
                return;
            __VLS_ctx.$router.push(__VLS_ctx.menu);
        }
    };
    __VLS_15.slots.default;
    {
        var __VLS_thisSlot = __VLS_15.slots.icon;
        var __VLS_20 = {}.IconFont;
        /** @type {[typeof __VLS_components.IconFont, typeof __VLS_components.iconFont, typeof __VLS_components.IconFont, typeof __VLS_components.iconFont, ]} */ ;
        // @ts-ignore
        var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20(__assign({ type: (__VLS_ctx.menu.meta.icon) }, { style: {} })));
        var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign({ type: (__VLS_ctx.menu.meta.icon) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_21), false));
    }
    (__VLS_ctx.menu.meta.title);
    var __VLS_15;
}
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            visible: visible,
        };
    },
    props: {
        menu: {
            type: Object,
            required: true,
        }
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    props: {
        menu: {
            type: Object,
            required: true,
        }
    },
});
; /* PartiallyEnd: #4569/main.vue */
