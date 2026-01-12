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
import { ref, watch } from "vue";
import { MdEditor } from "md-editor-v3";
import "md-editor-v3/lib/style.css";
var props = withDefaults(defineProps(), {
    content: "",
    visible: false,
    title: "",
});
var emits = defineEmits();
var text = ref(props.content);
watch(function () { return props.content; }, function (newContent) {
    text.value = newContent;
});
var hadleCloseModal = function () {
    text.value = "";
    emits("close");
};
var handleConfirm = function () {
    emits("confirm", text.value);
    text.value = "";
};
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    content: "",
    visible: false,
    title: "",
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
var __VLS_0 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign(__assign(__assign({ 'onClose': {} }, { 'onCancel': {} }), { 'onOk': {} }), { visible: (__VLS_ctx.visible), maskClosable: (false), closable: (false), width: (1000) })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onClose': {} }, { 'onCancel': {} }), { 'onOk': {} }), { visible: (__VLS_ctx.visible), maskClosable: (false), closable: (false), width: (1000) })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onClose: (__VLS_ctx.hadleCloseModal)
};
var __VLS_8 = {
    onCancel: (__VLS_ctx.hadleCloseModal)
};
var __VLS_9 = {
    onOk: (__VLS_ctx.handleConfirm)
};
var __VLS_10 = {};
__VLS_3.slots.default;
{
    var __VLS_thisSlot = __VLS_3.slots.title;
    (__VLS_ctx.title);
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "editor-container" }));
var __VLS_11 = {}.MdEditor;
/** @type {[typeof __VLS_components.MdEditor, ]} */ ;
// @ts-ignore
var __VLS_12 = __VLS_asFunctionalComponent(__VLS_11, new __VLS_11({
    modelValue: (__VLS_ctx.text),
    toolbarsExclude: (['image', 'mermaid', 'save', 'catalog', 'github']),
}));
var __VLS_13 = __VLS_12.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.text),
        toolbarsExclude: (['image', 'mermaid', 'save', 'catalog', 'github']),
    }], __VLS_functionalComponentArgsRest(__VLS_12), false));
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['editor-container']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            MdEditor: MdEditor,
            text: text,
            hadleCloseModal: hadleCloseModal,
            handleConfirm: handleConfirm,
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
