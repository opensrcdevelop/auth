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
import { watch } from "vue";
import { nextTick, ref } from "vue";
var props = withDefaults(defineProps(), {
    modelValue: "",
    placeholder: "请输入",
});
var emits = defineEmits();
var inputRef = ref();
var inputStatus = ref(false);
var inputText = ref(props.modelValue);
var handleEdit = function () {
    inputStatus.value = true;
    nextTick(function () {
        inputRef.value.focus();
    });
};
var handleInput = function (val) {
    inputText.value = val;
};
var handleBlur = function (ev) {
    var val = ev.target.value;
    emits("update:modelValue", val);
    inputStatus.value = false;
};
/** 输出完成监听 */
var timeout;
watch(inputText, function (newVal, oldVal) {
    clearTimeout(timeout);
    timeout = setTimeout(function () {
        inputStatus.value = false;
        emits("update:modelValue", newVal);
    }, 500);
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    modelValue: "",
    placeholder: "请输入",
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "input-text" }));
if (!__VLS_ctx.inputStatus) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "text-container" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "text" }));
    (props.modelValue);
    var __VLS_0 = {}.ATooltip;
    /** @type {[typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, ]} */ ;
    // @ts-ignore
    var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
        content: "编辑",
    }));
    var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{
            content: "编辑",
        }], __VLS_functionalComponentArgsRest(__VLS_1), false));
    __VLS_3.slots.default;
    var __VLS_4 = {}.IconEdit;
    /** @type {[typeof __VLS_components.IconEdit, typeof __VLS_components.iconEdit, ]} */ ;
    // @ts-ignore
    var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4(__assign({ 'onClick': {} }, { class: "edit-btn" })));
    var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { class: "edit-btn" })], __VLS_functionalComponentArgsRest(__VLS_5), false));
    var __VLS_8 = void 0;
    var __VLS_9 = void 0;
    var __VLS_10 = void 0;
    var __VLS_11 = {
        onClick: (__VLS_ctx.handleEdit)
    };
    var __VLS_7;
    var __VLS_3;
}
else {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_12 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign(__assign({ 'onInput': {} }, { 'onBlur': {} }), { ref: "inputRef", defaultValue: (props.modelValue), placeholder: (props.placeholder) })));
    var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign(__assign({ 'onInput': {} }, { 'onBlur': {} }), { ref: "inputRef", defaultValue: (props.modelValue), placeholder: (props.placeholder) })], __VLS_functionalComponentArgsRest(__VLS_13), false));
    var __VLS_16 = void 0;
    var __VLS_17 = void 0;
    var __VLS_18 = void 0;
    var __VLS_19 = {
        onInput: (__VLS_ctx.handleInput)
    };
    var __VLS_20 = {
        onBlur: (__VLS_ctx.handleBlur)
    };
    /** @type {typeof __VLS_ctx.inputRef} */ ;
    var __VLS_21 = {};
    var __VLS_15;
}
/** @type {__VLS_StyleScopedClasses['input-text']} */ ;
/** @type {__VLS_StyleScopedClasses['text-container']} */ ;
/** @type {__VLS_StyleScopedClasses['text']} */ ;
/** @type {__VLS_StyleScopedClasses['edit-btn']} */ ;
// @ts-ignore
var __VLS_22 = __VLS_21;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            inputRef: inputRef,
            inputStatus: inputStatus,
            handleEdit: handleEdit,
            handleInput: handleInput,
            handleBlur: handleBlur,
        };
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
; /* PartiallyEnd: #4569/main.vue */
