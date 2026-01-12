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
import { RouterView } from "vue-router";
import Siderbar from "@/layout/SideBar/index.vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";
var globalVariables = useGlobalVariablesStore();
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
var __VLS_0 = {}.ALayout;
/** @type {[typeof __VLS_components.ALayout, typeof __VLS_components.aLayout, typeof __VLS_components.ALayout, typeof __VLS_components.aLayout, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ class: "layout-container" })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ class: "layout-container" })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4 = {};
__VLS_3.slots.default;
var __VLS_5 = {}.ASpin;
/** @type {[typeof __VLS_components.ASpin, typeof __VLS_components.aSpin, typeof __VLS_components.ASpin, typeof __VLS_components.aSpin, ]} */ ;
// @ts-ignore
var __VLS_6 = __VLS_asFunctionalComponent(__VLS_5, new __VLS_5(__assign({ class: "spin" }, { tip: "处理中，请稍后...", loading: (__VLS_ctx.globalVariables.apiLoading) })));
var __VLS_7 = __VLS_6.apply(void 0, __spreadArray([__assign({ class: "spin" }, { tip: "处理中，请稍后...", loading: (__VLS_ctx.globalVariables.apiLoading) })], __VLS_functionalComponentArgsRest(__VLS_6), false));
__VLS_8.slots.default;
var __VLS_9 = {}.ALayout;
/** @type {[typeof __VLS_components.ALayout, typeof __VLS_components.aLayout, typeof __VLS_components.ALayout, typeof __VLS_components.aLayout, ]} */ ;
// @ts-ignore
var __VLS_10 = __VLS_asFunctionalComponent(__VLS_9, new __VLS_9({}));
var __VLS_11 = __VLS_10.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_10), false));
__VLS_12.slots.default;
/** @type {[typeof Siderbar, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(Siderbar, new Siderbar({}));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_13), false));
var __VLS_16 = {}.ALayoutContent;
/** @type {[typeof __VLS_components.ALayoutContent, typeof __VLS_components.aLayoutContent, typeof __VLS_components.ALayoutContent, typeof __VLS_components.aLayoutContent, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16(__assign({ class: "layout-main" }, { id: "main-container" })));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([__assign({ class: "layout-main" }, { id: "main-container" })], __VLS_functionalComponentArgsRest(__VLS_17), false));
__VLS_19.slots.default;
var __VLS_20 = {}.RouterView;
/** @type {[typeof __VLS_components.RouterView, typeof __VLS_components.routerView, typeof __VLS_components.RouterView, typeof __VLS_components.routerView, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({}));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_21), false));
{
    var __VLS_thisSlot = __VLS_23.slots.default;
    var _a = __VLS_getSlotParams(__VLS_thisSlot)[0], Component = _a.Component, route = _a.route;
    var __VLS_24 = {}.transition;
    /** @type {[typeof __VLS_components.Transition, typeof __VLS_components.transition, typeof __VLS_components.Transition, typeof __VLS_components.transition, ]} */ ;
    // @ts-ignore
    var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({
        name: "fade",
        mode: "out-in",
    }));
    var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([{
            name: "fade",
            mode: "out-in",
        }], __VLS_functionalComponentArgsRest(__VLS_25), false));
    __VLS_27.slots.default;
    var __VLS_28 = ((Component));
    // @ts-ignore
    var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
        key: (route.path),
    }));
    var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
            key: (route.path),
        }], __VLS_functionalComponentArgsRest(__VLS_29), false));
    var __VLS_27;
    __VLS_23.slots['' /* empty slot name completion */];
}
var __VLS_23;
var __VLS_19;
var __VLS_32 = {}.ABackTop;
/** @type {[typeof __VLS_components.ABackTop, typeof __VLS_components.aBackTop, typeof __VLS_components.ABackTop, typeof __VLS_components.aBackTop, ]} */ ;
// @ts-ignore
var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32(__assign({ targetContainer: "#main-container" }, { style: ({ position: 'absolute' }) })));
var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([__assign({ targetContainer: "#main-container" }, { style: ({ position: 'absolute' }) })], __VLS_functionalComponentArgsRest(__VLS_33), false));
__VLS_35.slots.default;
var __VLS_36 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
    type: "primary",
    shape: "circle",
    size: "large",
}));
var __VLS_38 = __VLS_37.apply(void 0, __spreadArray([{
        type: "primary",
        shape: "circle",
        size: "large",
    }], __VLS_functionalComponentArgsRest(__VLS_37), false));
__VLS_39.slots.default;
var __VLS_40 = {}.IconCaretUp;
/** @type {[typeof __VLS_components.IconCaretUp, typeof __VLS_components.iconCaretUp, ]} */ ;
// @ts-ignore
var __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({}));
var __VLS_42 = __VLS_41.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_41), false));
var __VLS_39;
var __VLS_35;
var __VLS_12;
var __VLS_8;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['layout-container']} */ ;
/** @type {__VLS_StyleScopedClasses['spin']} */ ;
/** @type {__VLS_StyleScopedClasses['layout-main']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            RouterView: RouterView,
            Siderbar: Siderbar,
            globalVariables: globalVariables,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
