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
import MenuItem from "./MenuItem.vue";
import { useRoute } from "vue-router";
import menuRoutes from "@/router/menuRoutes";
import { computed } from "vue";
var route = useRoute();
var defaultSelected = computed(function () {
    if (route.meta.parent) {
        return [route.meta.parent];
    }
    else {
        return [route.path];
    }
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
var __VLS_0 = {}.AScrollbar;
/** @type {[typeof __VLS_components.AScrollbar, typeof __VLS_components.aScrollbar, typeof __VLS_components.AScrollbar, typeof __VLS_components.aScrollbar, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ style: {} })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4 = {};
__VLS_3.slots.default;
var __VLS_5 = {}.AMenu;
/** @type {[typeof __VLS_components.AMenu, typeof __VLS_components.aMenu, typeof __VLS_components.AMenu, typeof __VLS_components.aMenu, ]} */ ;
// @ts-ignore
var __VLS_6 = __VLS_asFunctionalComponent(__VLS_5, new __VLS_5({
    theme: "light",
    defaultSelectedKeys: (__VLS_ctx.defaultSelected),
    breakpoint: "md",
}));
var __VLS_7 = __VLS_6.apply(void 0, __spreadArray([{
        theme: "light",
        defaultSelectedKeys: (__VLS_ctx.defaultSelected),
        breakpoint: "md",
    }], __VLS_functionalComponentArgsRest(__VLS_6), false));
__VLS_8.slots.default;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.menuRoutes)); _i < _a.length; _i++) {
    var menu = _a[_i][0];
    /** @type {[typeof MenuItem, typeof MenuItem, ]} */ ;
    // @ts-ignore
    var __VLS_9 = __VLS_asFunctionalComponent(MenuItem, new MenuItem({
        key: (menu.path),
        menu: (menu),
    }));
    var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
            key: (menu.path),
            menu: (menu),
        }], __VLS_functionalComponentArgsRest(__VLS_9), false));
}
var __VLS_8;
var __VLS_3;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            MenuItem: MenuItem,
            menuRoutes: menuRoutes,
            defaultSelected: defaultSelected,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
