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
var props = defineProps({
    priority: {
        type: Number,
        required: true,
    },
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
if (__VLS_ctx.priority === -1) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "priority-tag priority-tag-lowest" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
}
if (__VLS_ctx.priority === 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "priority-tag priority-tag-low" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
}
if (__VLS_ctx.priority === 1) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "priority-tag priority-tag-medium" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
}
if (__VLS_ctx.priority === 2) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "priority-tag priority-tag-high" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
}
if (__VLS_ctx.priority === 3) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "priority-tag priority-tag-highest" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
}
/** @type {__VLS_StyleScopedClasses['priority-tag']} */ ;
/** @type {__VLS_StyleScopedClasses['priority-tag-lowest']} */ ;
/** @type {__VLS_StyleScopedClasses['priority-tag']} */ ;
/** @type {__VLS_StyleScopedClasses['priority-tag-low']} */ ;
/** @type {__VLS_StyleScopedClasses['priority-tag']} */ ;
/** @type {__VLS_StyleScopedClasses['priority-tag-medium']} */ ;
/** @type {__VLS_StyleScopedClasses['priority-tag']} */ ;
/** @type {__VLS_StyleScopedClasses['priority-tag-high']} */ ;
/** @type {__VLS_StyleScopedClasses['priority-tag']} */ ;
/** @type {__VLS_StyleScopedClasses['priority-tag-highest']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    props: {
        priority: {
            type: Number,
            required: true,
        },
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    props: {
        priority: {
            type: Number,
            required: true,
        },
    },
});
; /* PartiallyEnd: #4569/main.vue */
