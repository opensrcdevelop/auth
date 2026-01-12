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
import { useEventSource } from "@/hooks/useEventSource";
import { nextTick, reactive, ref, watch } from "vue";
import { generateRandomString, handleApiError, handleApiSuccess, } from "@/util/tool";
import { getEnabledDataSourceConf, getEnabledModelProvider, getUserChatMessageHistory, } from "@/api/chatbi";
import { Message } from "@arco-design/web-vue";
import ChatMessage from "./components/ChatMessage.vue";
var props = withDefaults(defineProps(), {
    chatId: undefined,
    dataSourceId: undefined,
});
var emits = defineEmits();
var _a = useEventSource(), abort = _a.abort, fetchStream = _a.fetchStream;
var messageContainer = ref(null);
var messages = reactive([]);
var userInput = ref("");
var loading = ref(false);
var questionId = ref("");
var dataSourceList = reactive([]);
var modelProviderList = reactive([]);
var selectedDataSource = ref("");
var selectedModel = ref("");
var greetingText = ref("");
var activeChatId = ref("");
var init = function () {
    greetingText.value = greeting();
    activeChatId.value = "";
    messages.length = 0;
    // 获取已启用的数据源
    getEnabledDataSourceConf()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            dataSourceList.length = 0;
            dataSourceList.push.apply(dataSourceList, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取已启用的数据源");
    });
    // 获取已启用的模型提供商
    getEnabledModelProvider()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            modelProviderList.length = 0;
            modelProviderList.push.apply(modelProviderList, data);
            if (modelProviderList.length > 0) {
                var firstProvider = modelProviderList[0];
                selectedModel.value = "".concat(firstProvider.id, ":").concat(firstProvider.defaultModel);
            }
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取模型提供商列表");
    });
};
var __VLS_exposed = {
    init: init,
};
defineExpose(__VLS_exposed);
watch(function () { return props.chatId; }, function (newVal) {
    // 切换对话
    if (newVal && newVal !== activeChatId.value) {
        activeChatId.value = newVal;
        handleGetChatMessageHistory(newVal);
    }
    // 开启新对话
    if (!newVal) {
        activeChatId.value = "";
        selectedDataSource.value = "";
        messages.length = 0;
    }
});
watch(function () { return props.dataSourceId; }, function (newVal) {
    selectedDataSource.value = newVal;
});
/**
 * 获取对话消息历史
 */
var handleGetChatMessageHistory = function (chatId) {
    getUserChatMessageHistory(chatId).then(function (result) {
        handleApiSuccess(result, function (data) {
            messages.length = 0;
            data.forEach(function (item) {
                handleMessage(item);
            });
        });
    });
};
/**
 * 问候语
 */
var greeting = function () {
    var hour = new Date().getHours();
    if (hour < 6)
        return "夜深了，注意休息！有什么我可以帮助你的吗？";
    if (hour < 9)
        return "早上好！有什么我可以帮助你的吗？";
    if (hour < 12)
        return "上午好！有什么我可以帮助你的吗？";
    if (hour < 14)
        return "中午好！有什么我可以帮助你的吗？";
    if (hour < 17)
        return "下午好！有什么我可以帮助你的吗？";
    if (hour < 19)
        return "傍晚好！有什么我可以帮助你的吗？";
    return "晚上好！有什么我可以帮助你的吗？";
};
/**
 * 发送消息
 */
var sendMessage = function (input) {
    if (!selectedDataSource.value) {
        Message.warning("请选择数据源");
        return;
    }
    if (!selectedModel.value) {
        Message.warning("请选择大模型");
        return;
    }
    var question = input || userInput.value;
    if (!question.trim() || loading.value)
        return;
    questionId.value = generateRandomString(12);
    messages.push({
        role: "USER",
        type: "TEXT",
        content: question,
        questionId: questionId.value,
        chatId: activeChatId.value,
    });
    messages.push({
        role: "ASSISTANT",
        type: "LOADING",
        loading: true,
        content: "回答生成中...",
        questionId: questionId.value,
        error: false,
        chatId: activeChatId.value,
    });
    if (userInput.value) {
        userInput.value = "";
    }
    loading.value = true;
    fetchStream({
        url: "/chatbi/chat/stream",
        body: {
            question: question,
            questionId: questionId.value,
            modelProviderId: selectedModel.value.split(":")[0],
            model: selectedModel.value.split(":")[1],
            dataSourceId: selectedDataSource.value,
            chatId: activeChatId.value,
        },
        onMessage: function (message) { return handleMessage(message); },
        onError: function (error) {
            console.error(error);
            loading.value = false;
            var loadingItem = messages.find(function (item) {
                return item.questionId === questionId.value && item.type === "LOADING";
            });
            if (loadingItem) {
                loadingItem.loading = false;
                loadingItem.error = true;
                loadingItem.content = "发生了未知错误";
            }
            abort();
        },
        onClose: function () {
            loading.value = false;
        },
    });
    nextTick(function () {
        scrollToBottom();
    });
};
/**
 * 重新发送消息
 */
var resendMessage = function (qId) {
    var userQuestion = messages.find(function (item) { return item.questionId === qId && item.role === "USER"; });
    sendMessage(userQuestion.content);
};
/**
 * 处理消息
 */
var handleMessage = function (message) {
    scrollToBottom();
    activeChatId.value = message.chatId;
    // 如果当前对话为全新对话，则更新对话历史
    if (activeChatId.value !== props.chatId) {
        emits("updateChatHistory", activeChatId.value);
    }
    if (message.role === "USER") {
        messages.push(message);
        scrollToBottom();
        return;
    }
    if (message.type === "DONE") {
        loading.value = false;
        var loadingItem = messages.find(function (item) {
            return item.questionId === message.questionId && item.type === "LOADING";
        });
        if (loadingItem) {
            loadingItem.loading = false;
            loadingItem.content = "回答完成";
        }
        messages.push({
            role: "assistant",
            type: message.type,
            questionId: message.questionId,
            chatId: message.chatId,
            answerId: message.answerId,
            actionType: message.actionType,
            rewrittenQuestion: message.rewrittenQuestion,
            time: message.time,
            feedback: message.feedback,
        });
        scrollToBottom();
        return;
    }
    if (message.type === "LOADING") {
        var loadingItem = messages.find(function (item) {
            return item.questionId === message.questionId && item.type === "LOADING";
        });
        if (loadingItem) {
            loadingItem.content = message.content;
        }
        else {
            messages.push(message);
        }
        return;
    }
    if (message.type === "ERROR") {
        var loadingItem = messages.find(function (item) {
            return item.questionId === message.questionId && item.type === "LOADING";
        });
        if (loadingItem) {
            loadingItem.loading = false;
            loadingItem.error = true;
            loadingItem.content = message.content;
        }
        return;
    }
    if (messages.length > 0 &&
        messages[messages.length - 1].role === "assistant") {
        var last = messages[messages.length - 1];
        // 类型相同，合并内容
        if (last.type === message.type) {
            if (["MARKDOWN", "TEXT", "HTML_REPORT", "THINKING"].includes(message.type) && message.content) {
                last.content += message.content;
            }
            else if (["ECHARTS", "TABLE"].includes(message.type)) {
                last.content = message.content;
            }
        }
        else {
            // 类型不同追加新消息
            messages.push({
                role: "assistant",
                type: message.type,
                content: message.content,
                questionId: message.questionId,
                chatId: message.chatId,
            });
        }
    }
    else {
        messages.push({
            role: "assistant",
            type: message.type,
            content: message.content,
            questionId: message.questionId,
            chatId: message.chatId,
        });
    }
};
/**
 * 停止生成
 */
var stopGenerating = function (qId) {
    if (qId === void 0) { qId = questionId.value; }
    abort();
    loading.value = false;
    if (messages.length > 0) {
        var loadingItem = messages.find(function (item) { return item.questionId === qId && item.type === "LOADING"; });
        if (loadingItem) {
            loadingItem.loading = false;
            loadingItem.content = "回答已取消";
        }
    }
};
/**
 * 将消息容器滚动到底部
 */
var scrollToBottom = function () {
    nextTick(function () {
        if (messageContainer.value) {
            messageContainer.value.scrollTop = messageContainer.value.scrollHeight;
        }
    });
};
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    chatId: undefined,
    dataSourceId: undefined,
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "chat-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "message-container" }, { ref: "messageContainer" }));
/** @type {typeof __VLS_ctx.messageContainer} */ ;
if (!__VLS_ctx.messages.length) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "empty-container" }));
    (__VLS_ctx.greetingText);
}
/** @type {[typeof ChatMessage, ]} */ ;
// @ts-ignore
var __VLS_0 = __VLS_asFunctionalComponent(ChatMessage, new ChatMessage(__assign({ 'onSendMessage': {} }, { messages: (__VLS_ctx.messages) })));
var __VLS_1 = __VLS_0.apply(void 0, __spreadArray([__assign({ 'onSendMessage': {} }, { messages: (__VLS_ctx.messages) })], __VLS_functionalComponentArgsRest(__VLS_0), false));
var __VLS_3;
var __VLS_4;
var __VLS_5;
var __VLS_6 = {
    onSendMessage: (__VLS_ctx.sendMessage)
};
var __VLS_2;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "input-area" }));
var __VLS_7 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_8 = __VLS_asFunctionalComponent(__VLS_7, new __VLS_7(__assign(__assign({ 'onKeyup': {} }, { class: "no-border-textarea" }), { placeholder: "请输入您的问题...", autoSize: ({
        minRows: 3,
        maxRows: 5,
    }), modelValue: (__VLS_ctx.userInput) })));
var __VLS_9 = __VLS_8.apply(void 0, __spreadArray([__assign(__assign({ 'onKeyup': {} }, { class: "no-border-textarea" }), { placeholder: "请输入您的问题...", autoSize: ({
            minRows: 3,
            maxRows: 5,
        }), modelValue: (__VLS_ctx.userInput) })], __VLS_functionalComponentArgsRest(__VLS_8), false));
var __VLS_11;
var __VLS_12;
var __VLS_13;
var __VLS_14 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.sendMessage(__VLS_ctx.userInput);
    }
};
var __VLS_10;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "bottom-bar" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "option" }));
var __VLS_15 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_16 = __VLS_asFunctionalComponent(__VLS_15, new __VLS_15({}));
var __VLS_17 = __VLS_16.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_16), false));
__VLS_18.slots.default;
var __VLS_19 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_20 = __VLS_asFunctionalComponent(__VLS_19, new __VLS_19({
    placeholder: "请选择数据源",
    size: "mini",
    allowSearch: true,
    bordered: (false),
    modelValue: (__VLS_ctx.selectedDataSource),
}));
var __VLS_21 = __VLS_20.apply(void 0, __spreadArray([{
        placeholder: "请选择数据源",
        size: "mini",
        allowSearch: true,
        bordered: (false),
        modelValue: (__VLS_ctx.selectedDataSource),
    }], __VLS_functionalComponentArgsRest(__VLS_20), false));
__VLS_22.slots.default;
for (var _i = 0, _b = __VLS_getVForSourceType((__VLS_ctx.dataSourceList)); _i < _b.length; _i++) {
    var item = _b[_i][0];
    var __VLS_23 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_24 = __VLS_asFunctionalComponent(__VLS_23, new __VLS_23({
        key: (item.id),
        value: (item.id),
    }));
    var __VLS_25 = __VLS_24.apply(void 0, __spreadArray([{
            key: (item.id),
            value: (item.id),
        }], __VLS_functionalComponentArgsRest(__VLS_24), false));
    __VLS_26.slots.default;
    (item.name);
    var __VLS_26;
}
var __VLS_22;
var __VLS_27 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_28 = __VLS_asFunctionalComponent(__VLS_27, new __VLS_27(__assign({ placeholder: "请选择大模型", size: "mini", allowSearch: true, bordered: (false), modelValue: (__VLS_ctx.selectedModel) }, { style: {} })));
var __VLS_29 = __VLS_28.apply(void 0, __spreadArray([__assign({ placeholder: "请选择大模型", size: "mini", allowSearch: true, bordered: (false), modelValue: (__VLS_ctx.selectedModel) }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_28), false));
__VLS_30.slots.default;
for (var _c = 0, _d = __VLS_getVForSourceType((__VLS_ctx.modelProviderList)); _c < _d.length; _c++) {
    var item = _d[_c][0];
    var __VLS_31 = {}.AOptgroup;
    /** @type {[typeof __VLS_components.AOptgroup, typeof __VLS_components.aOptgroup, typeof __VLS_components.AOptgroup, typeof __VLS_components.aOptgroup, ]} */ ;
    // @ts-ignore
    var __VLS_32 = __VLS_asFunctionalComponent(__VLS_31, new __VLS_31({
        key: (item.id),
        label: (item.name),
    }));
    var __VLS_33 = __VLS_32.apply(void 0, __spreadArray([{
            key: (item.id),
            label: (item.name),
        }], __VLS_functionalComponentArgsRest(__VLS_32), false));
    __VLS_34.slots.default;
    for (var _e = 0, _f = __VLS_getVForSourceType((item.optionalModels)); _e < _f.length; _e++) {
        var _g = _f[_e], model = _g[0], index = _g[1];
        var __VLS_35 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_36 = __VLS_asFunctionalComponent(__VLS_35, new __VLS_35({
            key: (index),
            value: ("".concat(item.id, ":").concat(model.name)),
        }));
        var __VLS_37 = __VLS_36.apply(void 0, __spreadArray([{
                key: (index),
                value: ("".concat(item.id, ":").concat(model.name)),
            }], __VLS_functionalComponentArgsRest(__VLS_36), false));
        __VLS_38.slots.default;
        (model.name);
        var __VLS_38;
    }
    var __VLS_34;
}
var __VLS_30;
var __VLS_18;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
if (!__VLS_ctx.loading) {
    var __VLS_39 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_40 = __VLS_asFunctionalComponent(__VLS_39, new __VLS_39(__assign({ 'onClick': {} }, { type: "primary", shape: "circle" })));
    var __VLS_41 = __VLS_40.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary", shape: "circle" })], __VLS_functionalComponentArgsRest(__VLS_40), false));
    var __VLS_43 = void 0;
    var __VLS_44 = void 0;
    var __VLS_45 = void 0;
    var __VLS_46 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            if (!(!__VLS_ctx.loading))
                return;
            __VLS_ctx.sendMessage(__VLS_ctx.userInput);
        }
    };
    __VLS_42.slots.default;
    {
        var __VLS_thisSlot = __VLS_42.slots.icon;
        var __VLS_47 = {}.IconArrowUp;
        /** @type {[typeof __VLS_components.IconArrowUp, typeof __VLS_components.iconArrowUp, ]} */ ;
        // @ts-ignore
        var __VLS_48 = __VLS_asFunctionalComponent(__VLS_47, new __VLS_47({}));
        var __VLS_49 = __VLS_48.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_48), false));
    }
    var __VLS_42;
}
else {
    var __VLS_51 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_52 = __VLS_asFunctionalComponent(__VLS_51, new __VLS_51(__assign({ 'onClick': {} }, { type: "primary", shape: "circle" })));
    var __VLS_53 = __VLS_52.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary", shape: "circle" })], __VLS_functionalComponentArgsRest(__VLS_52), false));
    var __VLS_55 = void 0;
    var __VLS_56 = void 0;
    var __VLS_57 = void 0;
    var __VLS_58 = {
        onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            if (!!(!__VLS_ctx.loading))
                return;
            __VLS_ctx.stopGenerating();
        }
    };
    __VLS_54.slots.default;
    {
        var __VLS_thisSlot = __VLS_54.slots.icon;
        var __VLS_59 = {}.IconRecordStop;
        /** @type {[typeof __VLS_components.IconRecordStop, typeof __VLS_components.iconRecordStop, ]} */ ;
        // @ts-ignore
        var __VLS_60 = __VLS_asFunctionalComponent(__VLS_59, new __VLS_59({}));
        var __VLS_61 = __VLS_60.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_60), false));
    }
    var __VLS_54;
}
/** @type {__VLS_StyleScopedClasses['chat-container']} */ ;
/** @type {__VLS_StyleScopedClasses['message-container']} */ ;
/** @type {__VLS_StyleScopedClasses['empty-container']} */ ;
/** @type {__VLS_StyleScopedClasses['input-area']} */ ;
/** @type {__VLS_StyleScopedClasses['no-border-textarea']} */ ;
/** @type {__VLS_StyleScopedClasses['bottom-bar']} */ ;
/** @type {__VLS_StyleScopedClasses['option']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            ChatMessage: ChatMessage,
            messageContainer: messageContainer,
            messages: messages,
            userInput: userInput,
            loading: loading,
            dataSourceList: dataSourceList,
            modelProviderList: modelProviderList,
            selectedDataSource: selectedDataSource,
            selectedModel: selectedModel,
            greetingText: greetingText,
            sendMessage: sendMessage,
            stopGenerating: stopGenerating,
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
