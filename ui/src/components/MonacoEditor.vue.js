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
import { useMonacoEditor } from "@/hooks/useMonacoEditor";
import { computed, onMounted, watch } from "vue";
var props = withDefaults(defineProps(), {
    width: "100%",
    height: "100%",
    editorOption: function () { return ({}); },
    modelValue: "",
});
var emits = defineEmits();
var monacoEditorStyle = computed(function () {
    return {
        width: typeof props.width === "string" ? props.width : props.width + "px",
        height: typeof props.height === "string" ? props.height : props.height + "px",
        border: "1px solid var(--color-neutral-2)"
    };
});
var _a = useMonacoEditor(props.language), monacoEditorRef = _a.monacoEditorRef, createEditor = _a.createEditor, updateVal = _a.updateVal, updateOptions = _a.updateOptions, getEditor = _a.getEditor;
function updateMonacoVal(val) {
    if (val !== getEditor().getValue()) {
        updateVal(val);
    }
}
onMounted(function () {
    var monacoEditor = createEditor();
    updateMonacoVal(props.modelValue);
    updateOptions(props.editorOption);
    monacoEditor === null || monacoEditor === void 0 ? void 0 : monacoEditor.onDidChangeModelContent(function () {
        emits("update:modelValue", monacoEditor.getValue());
    });
    monacoEditor === null || monacoEditor === void 0 ? void 0 : monacoEditor.onDidBlurEditorText(function () {
        emits("blur");
    });
});
watch(function () { return props.modelValue; }, function () {
    updateMonacoVal(props.modelValue);
});
var __VLS_exposed = { updateOptions: updateOptions };
defineExpose(__VLS_exposed);
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    width: "100%",
    height: "100%",
    editorOption: function () { return ({}); },
    modelValue: "",
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ ref: "monacoEditorRef" }, { style: (__VLS_ctx.monacoEditorStyle) }));
/** @type {typeof __VLS_ctx.monacoEditorRef} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            monacoEditorStyle: monacoEditorStyle,
            monacoEditorRef: monacoEditorRef,
        };
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return __assign({}, __VLS_exposed);
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
; /* PartiallyEnd: #4569/main.vue */
