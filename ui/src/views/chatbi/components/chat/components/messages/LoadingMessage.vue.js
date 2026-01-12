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
var __VLS_props = withDefaults(defineProps(), {
    message: {},
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    message: {},
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
if (__VLS_ctx.message.type === 'LOADING') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "avatar-container" }));
    var __VLS_0 = {}.AAvatar;
    /** @type {[typeof __VLS_components.AAvatar, typeof __VLS_components.aAvatar, typeof __VLS_components.AAvatar, typeof __VLS_components.aAvatar, ]} */ ;
    // @ts-ignore
    var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ class: "avatar-assistant" })));
    var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ class: "avatar-assistant" })], __VLS_functionalComponentArgsRest(__VLS_1), false));
    __VLS_3.slots.default;
    var __VLS_4 = {}.IconFont;
    /** @type {[typeof __VLS_components.IconFont, typeof __VLS_components.iconFont, typeof __VLS_components.IconFont, typeof __VLS_components.iconFont, ]} */ ;
    // @ts-ignore
    var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({
        type: "icon-assistant",
    }));
    var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([{
            type: "icon-assistant",
        }], __VLS_functionalComponentArgsRest(__VLS_5), false));
    var __VLS_3;
    var __VLS_8 = {}.ATag;
    /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
    // @ts-ignore
    var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
        color: (__VLS_ctx.message.error ? 'red' : 'arcoblue'),
    }));
    var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
            color: (__VLS_ctx.message.error ? 'red' : 'arcoblue'),
        }], __VLS_functionalComponentArgsRest(__VLS_9), false));
    __VLS_11.slots.default;
    {
        var __VLS_thisSlot = __VLS_11.slots.icon;
        if (__VLS_ctx.message.error) {
            var __VLS_12 = {}.IconCloseCircle;
            /** @type {[typeof __VLS_components.IconCloseCircle, typeof __VLS_components.iconCloseCircle, ]} */ ;
            // @ts-ignore
            var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({}));
            var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_13), false));
        }
        if (!__VLS_ctx.message.error && __VLS_ctx.message.loading) {
            var __VLS_16 = {}.IconLoading;
            /** @type {[typeof __VLS_components.IconLoading, typeof __VLS_components.iconLoading, ]} */ ;
            // @ts-ignore
            var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({}));
            var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_17), false));
        }
        if (!__VLS_ctx.message.error && !__VLS_ctx.message.loading) {
            var __VLS_20 = {}.IconCheckCircle;
            /** @type {[typeof __VLS_components.IconCheckCircle, typeof __VLS_components.iconCheckCircle, ]} */ ;
            // @ts-ignore
            var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({}));
            var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_21), false));
        }
    }
    (__VLS_ctx.message.content);
    var __VLS_11;
}
/** @type {__VLS_StyleScopedClasses['avatar-container']} */ ;
/** @type {__VLS_StyleScopedClasses['avatar-assistant']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    __typeProps: {},
    props: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    __typeProps: {},
    props: {},
});
; /* PartiallyEnd: #4569/main.vue */
