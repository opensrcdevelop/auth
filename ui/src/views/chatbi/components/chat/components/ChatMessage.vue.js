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
import ChartMessage from "./messages/ChartMessage.vue";
import DoneMessage from "./messages/DoneMessage.vue";
import HtmlReportMessage from "./messages/HtmlReportMessage.vue";
import LoadingMessage from "./messages/LoadingMessage.vue";
import TableMessage from "./messages/TableMessage.vue";
import TextMessage from "./messages/TextMessage.vue";
import MarkdownMessage from "./messages/MarkdownMessage.vue";
import ThinkingMessage from "./messages/ThinkingMessage.vue";
var props = withDefaults(defineProps(), {
    messages: function () { return []; },
});
var emits = defineEmits();
var handleResendMessage = function (question) {
    emits("sendMessage", question);
};
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    messages: function () { return []; },
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.messages)); _i < _a.length; _i++) {
    var _b = _a[_i], message = _b[0], index = _b[1];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ key: (index) }, { class: ([
            'message',
            message.role === 'USER' ? 'user-message' : 'assistant-message',
            message.role === 'USER' && index !== 0 ? 'user-message-mt' : '',
        ]) }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: ([
            'message-content',
            message.role === 'USER'
                ? 'user-message-content'
                : 'assistant-message-content',
        ]) }));
    /** @type {[typeof LoadingMessage, ]} */ ;
    // @ts-ignore
    var __VLS_0 = __VLS_asFunctionalComponent(LoadingMessage, new LoadingMessage({
        message: (message),
    }));
    var __VLS_1 = __VLS_0.apply(void 0, __spreadArray([{
            message: (message),
        }], __VLS_functionalComponentArgsRest(__VLS_0), false));
    /** @type {[typeof TextMessage, ]} */ ;
    // @ts-ignore
    var __VLS_3 = __VLS_asFunctionalComponent(TextMessage, new TextMessage({
        message: (message),
    }));
    var __VLS_4 = __VLS_3.apply(void 0, __spreadArray([{
            message: (message),
        }], __VLS_functionalComponentArgsRest(__VLS_3), false));
    /** @type {[typeof MarkdownMessage, ]} */ ;
    // @ts-ignore
    var __VLS_6 = __VLS_asFunctionalComponent(MarkdownMessage, new MarkdownMessage({
        message: (message),
    }));
    var __VLS_7 = __VLS_6.apply(void 0, __spreadArray([{
            message: (message),
        }], __VLS_functionalComponentArgsRest(__VLS_6), false));
    /** @type {[typeof ChartMessage, ]} */ ;
    // @ts-ignore
    var __VLS_9 = __VLS_asFunctionalComponent(ChartMessage, new ChartMessage({
        message: (message),
    }));
    var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
            message: (message),
        }], __VLS_functionalComponentArgsRest(__VLS_9), false));
    /** @type {[typeof TableMessage, ]} */ ;
    // @ts-ignore
    var __VLS_12 = __VLS_asFunctionalComponent(TableMessage, new TableMessage({
        message: (message),
    }));
    var __VLS_13 = __VLS_12.apply(void 0, __spreadArray([{
            message: (message),
        }], __VLS_functionalComponentArgsRest(__VLS_12), false));
    /** @type {[typeof HtmlReportMessage, ]} */ ;
    // @ts-ignore
    var __VLS_15 = __VLS_asFunctionalComponent(HtmlReportMessage, new HtmlReportMessage({
        message: (message),
    }));
    var __VLS_16 = __VLS_15.apply(void 0, __spreadArray([{
            message: (message),
        }], __VLS_functionalComponentArgsRest(__VLS_15), false));
    /** @type {[typeof ThinkingMessage, ]} */ ;
    // @ts-ignore
    var __VLS_18 = __VLS_asFunctionalComponent(ThinkingMessage, new ThinkingMessage({
        message: (message),
    }));
    var __VLS_19 = __VLS_18.apply(void 0, __spreadArray([{
            message: (message),
        }], __VLS_functionalComponentArgsRest(__VLS_18), false));
    /** @type {[typeof DoneMessage, ]} */ ;
    // @ts-ignore
    var __VLS_21 = __VLS_asFunctionalComponent(DoneMessage, new DoneMessage(__assign({ 'onResendMessage': {} }, { message: (message) })));
    var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign({ 'onResendMessage': {} }, { message: (message) })], __VLS_functionalComponentArgsRest(__VLS_21), false));
    var __VLS_24 = void 0;
    var __VLS_25 = void 0;
    var __VLS_26 = void 0;
    var __VLS_27 = {
        onResendMessage: (__VLS_ctx.handleResendMessage)
    };
    var __VLS_23;
}
/** @type {__VLS_StyleScopedClasses['message']} */ ;
/** @type {__VLS_StyleScopedClasses['message-content']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            ChartMessage: ChartMessage,
            DoneMessage: DoneMessage,
            HtmlReportMessage: HtmlReportMessage,
            LoadingMessage: LoadingMessage,
            TableMessage: TableMessage,
            TextMessage: TextMessage,
            MarkdownMessage: MarkdownMessage,
            ThinkingMessage: ThinkingMessage,
            handleResendMessage: handleResendMessage,
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
