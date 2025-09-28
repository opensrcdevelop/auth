import router from "@/router";
import {defineComponent, onMounted, ref} from "vue";
import Chat from "./components/chat/Chat.vue";
import DataSourceManagement from "./components/datasource/DataSourceManagement.vue";
import {getQueryString} from "@/util/tool";
import ChatHistory from "./components/chat/ChatHistory.vue";

const activeTab = ref("chat");
const chatRef = ref();
const chatHistoryRef = ref();
const dataSourceManagementRef = ref();

/**
 * tab 切换事件
 */
const handleTabChange = (tabKey: string) => {
  router.replace({
    query: {
      ...router.currentRoute.value.query,
      active_tab: tabKey,
    },
  });
  activeTab.value = tabKey;
  handleTabInit(tabKey);
};

/**
 * tab 初始化
 */
const handleTabInit = (tabKey: string) => {
  switch (tabKey) {
    case "chat":
      chatRef.value?.init();
      chatHistoryRef.value?.init();
      break;
    case "data_source_management":
      dataSourceManagementRef.value?.init();
      break;
  }
};

/**
 * 切换对话
 */
const chatId = ref("");
const handleSwitchChat = (id: string) => {
  chatId.value = id;
};

/**
 * 添加新对话
 */
const handleAddNewChat = () => {
  chatId.value = "";
  dataSourceId.value = "";
};

/**
 * 更新对话历史
 */
const handleUpdateChatHistory = (id: string) => {
  chatId.value = id;
  chatHistoryRef.value?.init(id);
};

/**
 * 更新水数据源ID
 */
const dataSourceId = ref("");
const handleUpdateDataSourceId = (id: string) => {
  dataSourceId.value = id;
};

export default defineComponent({
  components: {
    Chat,
    ChatHistory,
    DataSourceManagement,
  },
  setup() {
    onMounted(() => {
      activeTab.value = getQueryString("active_tab") || "chat";
      handleTabInit(activeTab.value);
    });

    return {
      activeTab,
      handleTabChange,
      chatRef,
      chatHistoryRef,
      dataSourceManagementRef,
      chatId,
      handleSwitchChat,
      handleUpdateChatHistory,
      handleAddNewChat,
      dataSourceId,
      handleUpdateDataSourceId,
    };
  },
});
