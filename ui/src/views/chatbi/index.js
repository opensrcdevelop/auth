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
import router from "@/router";
import { defineComponent, onMounted, ref } from "vue";
import Chat from "./components/chat/Chat.vue";
import DataSourceManagement from "./components/datasource/DataSourceManagement.vue";
import { getQueryString } from "@/util/tool";
import ChatHistory from "./components/chat/ChatHistory.vue";
import LLMManagement from "./components/llm/LLMManagement.vue";
var activeTab = ref("chat");
var chatRef = ref();
var chatHistoryRef = ref();
var dataSourceManagementRef = ref();
var llmManagementRef = ref();
/**
 * tab 切换事件
 */
var handleTabChange = function (tabKey) {
    router.replace({
        query: __assign(__assign({}, router.currentRoute.value.query), { active_tab: tabKey }),
    });
    activeTab.value = tabKey;
    handleTabInit(tabKey);
};
/**
 * tab 初始化
 */
var handleTabInit = function (tabKey) {
    var _a, _b, _c, _d;
    switch (tabKey) {
        case "chat":
            chatId.value = "";
            (_a = chatRef.value) === null || _a === void 0 ? void 0 : _a.init();
            (_b = chatHistoryRef.value) === null || _b === void 0 ? void 0 : _b.init();
            break;
        case "data_source_management":
            (_c = dataSourceManagementRef.value) === null || _c === void 0 ? void 0 : _c.init();
            break;
        case "llm_management":
            (_d = llmManagementRef.value) === null || _d === void 0 ? void 0 : _d.init();
            break;
    }
};
/**
 * 切换对话
 */
var chatId = ref("");
var handleSwitchChat = function (id) {
    chatId.value = id;
};
/**
 * 添加新对话
 */
var handleAddNewChat = function () {
    chatId.value = "";
    dataSourceId.value = "";
};
/**
 * 更新对话历史
 */
var handleUpdateChatHistory = function (id) {
    var _a;
    chatId.value = id;
    (_a = chatHistoryRef.value) === null || _a === void 0 ? void 0 : _a.init(id);
};
/**
 * 更新数据源ID
 */
var dataSourceId = ref("");
var handleUpdateDataSourceId = function (id) {
    dataSourceId.value = id;
};
export default defineComponent({
    components: {
        Chat: Chat,
        ChatHistory: ChatHistory,
        DataSourceManagement: DataSourceManagement,
        LLMManagement: LLMManagement,
    },
    setup: function () {
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "chat";
            handleTabInit(activeTab.value);
        });
        return {
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            chatRef: chatRef,
            chatHistoryRef: chatHistoryRef,
            dataSourceManagementRef: dataSourceManagementRef,
            chatId: chatId,
            handleSwitchChat: handleSwitchChat,
            handleUpdateChatHistory: handleUpdateChatHistory,
            handleAddNewChat: handleAddNewChat,
            dataSourceId: dataSourceId,
            handleUpdateDataSourceId: handleUpdateDataSourceId,
            llmManagementRef: llmManagementRef
        };
    },
});
