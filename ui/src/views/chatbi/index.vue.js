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
import indexTs from "./index";
export default indexTs;
debugger; /* PartiallyEnd: #3632/script.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "chatbi-header" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "left" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
var __VLS_0 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onChange: (__VLS_ctx.handleTabChange)
};
__VLS_3.slots.default;
var __VLS_8 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    key: "chat",
    title: "对话问答",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        key: "chat",
        title: "对话问答",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
__VLS_11.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "chat-container" }));
var __VLS_12 = {}.ChatHistory;
/** @type {[typeof __VLS_components.ChatHistory, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign(__assign(__assign({ 'onSwitchChat': {} }, { 'onAddNewChat': {} }), { 'onUpdateDataSourceId': {} }), { ref: "chatHistoryRef" })));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onSwitchChat': {} }, { 'onAddNewChat': {} }), { 'onUpdateDataSourceId': {} }), { ref: "chatHistoryRef" })], __VLS_functionalComponentArgsRest(__VLS_13), false));
var __VLS_16;
var __VLS_17;
var __VLS_18;
var __VLS_19 = {
    onSwitchChat: (__VLS_ctx.handleSwitchChat)
};
var __VLS_20 = {
    onAddNewChat: (__VLS_ctx.handleAddNewChat)
};
var __VLS_21 = {
    onUpdateDataSourceId: (__VLS_ctx.handleUpdateDataSourceId)
};
/** @type {typeof __VLS_ctx.chatHistoryRef} */ ;
var __VLS_22 = {};
var __VLS_15;
var __VLS_24 = {}.Chat;
/** @type {[typeof __VLS_components.Chat, ]} */ ;
// @ts-ignore
var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24(__assign({ 'onUpdateChatHistory': {} }, { ref: "chatRef", chatId: (__VLS_ctx.chatId), dataSourceId: (__VLS_ctx.dataSourceId) })));
var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([__assign({ 'onUpdateChatHistory': {} }, { ref: "chatRef", chatId: (__VLS_ctx.chatId), dataSourceId: (__VLS_ctx.dataSourceId) })], __VLS_functionalComponentArgsRest(__VLS_25), false));
var __VLS_28;
var __VLS_29;
var __VLS_30;
var __VLS_31 = {
    onUpdateChatHistory: (__VLS_ctx.handleUpdateChatHistory)
};
/** @type {typeof __VLS_ctx.chatRef} */ ;
var __VLS_32 = {};
var __VLS_27;
var __VLS_11;
var __VLS_34 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
    key: "data_source_management",
    title: "数据源管理",
}));
var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
        key: "data_source_management",
        title: "数据源管理",
    }], __VLS_functionalComponentArgsRest(__VLS_35), false));
__VLS_37.slots.default;
var __VLS_38 = {}.DataSourceManagement;
/** @type {[typeof __VLS_components.DataSourceManagement, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
    ref: "dataSourceManagementRef",
}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
        ref: "dataSourceManagementRef",
    }], __VLS_functionalComponentArgsRest(__VLS_39), false));
/** @type {typeof __VLS_ctx.dataSourceManagementRef} */ ;
var __VLS_42 = {};
var __VLS_41;
var __VLS_37;
var __VLS_44 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_45 = __VLS_asFunctionalComponent(__VLS_44, new __VLS_44({
    key: "llm_management",
    title: "LLM 管理",
}));
var __VLS_46 = __VLS_45.apply(void 0, __spreadArray([{
        key: "llm_management",
        title: "LLM 管理",
    }], __VLS_functionalComponentArgsRest(__VLS_45), false));
__VLS_47.slots.default;
var __VLS_48 = {}.LLMManagement;
/** @type {[typeof __VLS_components.LLMManagement, ]} */ ;
// @ts-ignore
var __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({
    ref: "llmManagementRef",
}));
var __VLS_50 = __VLS_49.apply(void 0, __spreadArray([{
        ref: "llmManagementRef",
    }], __VLS_functionalComponentArgsRest(__VLS_49), false));
/** @type {typeof __VLS_ctx.llmManagementRef} */ ;
var __VLS_52 = {};
var __VLS_51;
var __VLS_47;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['chatbi-header']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['chat-container']} */ ;
// @ts-ignore
var __VLS_23 = __VLS_22, __VLS_33 = __VLS_32, __VLS_43 = __VLS_42, __VLS_53 = __VLS_52;
var __VLS_dollars;
var __VLS_self;
