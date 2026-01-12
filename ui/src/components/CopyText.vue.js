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
import { ref } from "vue";
var props = defineProps({
    text: {
        type: String,
        default: "",
    },
    textColor: {
        type: String,
        default: "#1d2129",
        required: false,
    },
});
var tipText = ref("点击复制");
var copyText = function () {
    if (navigator.clipboard && window.isSecureContext) {
        navigator.clipboard.writeText(props.text);
        tipText.value = "已复制到剪切板";
    }
    else {
        // 创建 text area
        var textArea = document.createElement("textarea");
        textArea.value = props.text;
        // 使 text area 不在 viewport，同时设置不可见
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        // 执行复制命令并移除文本框
        document.execCommand("copy");
        textArea.remove();
        tipText.value = "已复制到剪切板";
    }
    setTimeout(function () {
        tipText.value = "点击复制";
    }, 1000);
};
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
var __VLS_0 = {}.ATooltip;
/** @type {[typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
    content: (__VLS_ctx.tipText),
    position: "top",
}));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{
        content: (__VLS_ctx.tipText),
        position: "top",
    }], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4 = {};
__VLS_3.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: (__VLS_ctx.copyText) }, { class: "text-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ style: ({ color: props.textColor }) }));
(props.text);
var __VLS_5 = {}.IconCopy;
/** @type {[typeof __VLS_components.IconCopy, typeof __VLS_components.iconCopy, ]} */ ;
// @ts-ignore
var __VLS_6 = __VLS_asFunctionalComponent(__VLS_5, new __VLS_5(__assign({ class: "icon" })));
var __VLS_7 = __VLS_6.apply(void 0, __spreadArray([__assign({ class: "icon" })], __VLS_functionalComponentArgsRest(__VLS_6), false));
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['text-container']} */ ;
/** @type {__VLS_StyleScopedClasses['icon']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            tipText: tipText,
            copyText: copyText,
        };
    },
    props: {
        text: {
            type: String,
            default: "",
        },
        textColor: {
            type: String,
            default: "#1d2129",
            required: false,
        },
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    props: {
        text: {
            type: String,
            default: "",
        },
        textColor: {
            type: String,
            default: "#1d2129",
            required: false,
        },
    },
});
; /* PartiallyEnd: #4569/main.vue */
