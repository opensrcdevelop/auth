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
import { computed, ref } from "vue";
var props = defineProps({
    type: {
        type: String,
        required: true,
    },
    loading: {
        type: Boolean,
        required: false,
    },
    checkRes: {
        type: Object,
        required: false,
    },
    placeholder: {
        type: String,
        required: false,
    },
});
var emits = defineEmits();
var password = ref("");
var isError = computed(function () {
    var _a;
    if (password.value.length === 0) {
        return false;
    }
    return ((_a = props.checkRes) === null || _a === void 0 ? void 0 : _a.valid) === false;
});
var errorText = computed(function () {
    var _a;
    if ((_a = props.checkRes) === null || _a === void 0 ? void 0 : _a.errorMessage) {
        return props.checkRes.errorMessage;
    }
    return "";
});
var hasRuleResults = computed(function () {
    var _a;
    if ((_a = props.checkRes) === null || _a === void 0 ? void 0 : _a.ruleResults) {
        return props.checkRes.ruleResults.length > 0;
    }
    return false;
});
var doCheck = function () {
    if (password.value) {
        emits("check", password.value);
    }
};
var setPassword = function (value) {
    password.value = value;
    doCheck();
};
var __VLS_exposed = {
    setPassword: setPassword,
};
defineExpose(__VLS_exposed);
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "input-container" }));
if (__VLS_ctx.type === 'password') {
    var __VLS_0 = {}.AInputPassword;
    /** @type {[typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, ]} */ ;
    // @ts-ignore
    var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onInput': {} }, { modelValue: (__VLS_ctx.password), placeholder: (props.placeholder), error: (__VLS_ctx.isError) })));
    var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onInput': {} }, { modelValue: (__VLS_ctx.password), placeholder: (props.placeholder), error: (__VLS_ctx.isError) })], __VLS_functionalComponentArgsRest(__VLS_1), false));
    var __VLS_4 = void 0;
    var __VLS_5 = void 0;
    var __VLS_6 = void 0;
    var __VLS_7 = {
        onInput: (__VLS_ctx.doCheck)
    };
    __VLS_3.slots.default;
    {
        var __VLS_thisSlot = __VLS_3.slots.suffix;
        if (__VLS_ctx.loading) {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "icon" }, { style: {} }));
            var __VLS_8 = {}.IconLoading;
            /** @type {[typeof __VLS_components.IconLoading, typeof __VLS_components.iconLoading, ]} */ ;
            // @ts-ignore
            var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({}));
            var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_9), false));
        }
        else {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "icon" }, { style: {} }));
            if (!__VLS_ctx.isError && __VLS_ctx.password) {
                var __VLS_12 = {}.IconCheckCircle;
                /** @type {[typeof __VLS_components.IconCheckCircle, typeof __VLS_components.iconCheckCircle, ]} */ ;
                // @ts-ignore
                var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign({ style: {} })));
                var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_13), false));
            }
            if (__VLS_ctx.isError && __VLS_ctx.password) {
                var __VLS_16 = {}.IconCloseCircle;
                /** @type {[typeof __VLS_components.IconCloseCircle, typeof __VLS_components.iconCloseCircle, ]} */ ;
                // @ts-ignore
                var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16(__assign({ style: {} })));
                var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_17), false));
            }
        }
    }
    var __VLS_3;
}
if (__VLS_ctx.type === 'text') {
    var __VLS_20 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20(__assign({ 'onInput': {} }, { modelValue: (__VLS_ctx.password), placeholder: (props.placeholder), error: (__VLS_ctx.isError) })));
    var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign({ 'onInput': {} }, { modelValue: (__VLS_ctx.password), placeholder: (props.placeholder), error: (__VLS_ctx.isError) })], __VLS_functionalComponentArgsRest(__VLS_21), false));
    var __VLS_24 = void 0;
    var __VLS_25 = void 0;
    var __VLS_26 = void 0;
    var __VLS_27 = {
        onInput: (__VLS_ctx.doCheck)
    };
    __VLS_23.slots.default;
    {
        var __VLS_thisSlot = __VLS_23.slots.suffix;
        if (__VLS_ctx.loading) {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "icon" }));
            var __VLS_28 = {}.IconLoading;
            /** @type {[typeof __VLS_components.IconLoading, typeof __VLS_components.iconLoading, ]} */ ;
            // @ts-ignore
            var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({}));
            var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_29), false));
        }
        else {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "icon" }));
            if (!__VLS_ctx.isError && __VLS_ctx.password) {
                var __VLS_32 = {}.IconCheckCircle;
                /** @type {[typeof __VLS_components.IconCheckCircle, typeof __VLS_components.iconCheckCircle, ]} */ ;
                // @ts-ignore
                var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32(__assign({ style: {} })));
                var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_33), false));
            }
            if (__VLS_ctx.isError && __VLS_ctx.password) {
                var __VLS_36 = {}.IconCloseCircle;
                /** @type {[typeof __VLS_components.IconCloseCircle, typeof __VLS_components.iconCloseCircle, ]} */ ;
                // @ts-ignore
                var __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36(__assign({ style: {} })));
                var __VLS_38 = __VLS_37.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_37), false));
            }
        }
    }
    var __VLS_23;
}
if (__VLS_ctx.isError && !__VLS_ctx.hasRuleResults) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "error-text" }));
    (__VLS_ctx.errorText);
}
var __VLS_40 = {}.transition;
/** @type {[typeof __VLS_components.Transition, typeof __VLS_components.transition, typeof __VLS_components.Transition, typeof __VLS_components.transition, ]} */ ;
// @ts-ignore
var __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40({
    name: "slide-down",
}));
var __VLS_42 = __VLS_41.apply(void 0, __spreadArray([{
        name: "slide-down",
    }], __VLS_functionalComponentArgsRest(__VLS_41), false));
__VLS_43.slots.default;
if (__VLS_ctx.isError && __VLS_ctx.hasRuleResults) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "rule-result" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "rule-text" }));
    for (var _i = 0, _b = __VLS_getVForSourceType(((_a = props.checkRes) === null || _a === void 0 ? void 0 : _a.ruleResults)); _i < _b.length; _i++) {
        var rule = _b[_i][0];
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
            key: (rule.rule),
        });
        if (rule.valid) {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "rule-text" }));
            var __VLS_44 = {}.IconCheckCircle;
            /** @type {[typeof __VLS_components.IconCheckCircle, typeof __VLS_components.iconCheckCircle, ]} */ ;
            // @ts-ignore
            var __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44(__assign({ style: {} })));
            var __VLS_46 = __VLS_45.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_45), false));
            (rule.rule);
        }
        else {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "rule-text" }));
            var __VLS_48 = {}.IconCloseCircle;
            /** @type {[typeof __VLS_components.IconCloseCircle, typeof __VLS_components.iconCloseCircle, ]} */ ;
            // @ts-ignore
            var __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48(__assign({ style: {} })));
            var __VLS_50 = __VLS_49.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_49), false));
            (rule.rule);
        }
    }
}
var __VLS_43;
/** @type {__VLS_StyleScopedClasses['input-container']} */ ;
/** @type {__VLS_StyleScopedClasses['icon']} */ ;
/** @type {__VLS_StyleScopedClasses['icon']} */ ;
/** @type {__VLS_StyleScopedClasses['icon']} */ ;
/** @type {__VLS_StyleScopedClasses['icon']} */ ;
/** @type {__VLS_StyleScopedClasses['error-text']} */ ;
/** @type {__VLS_StyleScopedClasses['rule-result']} */ ;
/** @type {__VLS_StyleScopedClasses['rule-text']} */ ;
/** @type {__VLS_StyleScopedClasses['rule-text']} */ ;
/** @type {__VLS_StyleScopedClasses['rule-text']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            password: password,
            isError: isError,
            errorText: errorText,
            hasRuleResults: hasRuleResults,
            doCheck: doCheck,
        };
    },
    __typeEmits: {},
    props: {
        type: {
            type: String,
            required: true,
        },
        loading: {
            type: Boolean,
            required: false,
        },
        checkRes: {
            type: Object,
            required: false,
        },
        placeholder: {
            type: String,
            required: false,
        },
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return __assign({}, __VLS_exposed);
    },
    __typeEmits: {},
    props: {
        type: {
            type: String,
            required: true,
        },
        loading: {
            type: Boolean,
            required: false,
        },
        checkRes: {
            type: Object,
            required: false,
        },
        placeholder: {
            type: String,
            required: false,
        },
    },
});
; /* PartiallyEnd: #4569/main.vue */
