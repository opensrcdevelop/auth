import router from "@/router";
import {defineComponent, onMounted, ref} from "vue";
import Chat from "./components/chat/Chat.vue";
import ChatSettings from "./components/settings/ChatSettings.vue";
import DataSourceManagement from "./components/datasource/DataSourceManagement.vue";
import {getQueryString} from "@/util/tool";
import ChatHistory from "./components/chat/ChatHistory.vue";
import LLMManagement from "./components/llm/LLMManagement.vue";
import SampleSqlManagement from "./components/sampleSql/SampleSqlManagement.vue";

const activeTab = ref("chat");
const chatRef = ref();
const chatSettingsRef = ref();
const chatHistoryRef = ref();
const dataSourceManagementRef = ref();
const llmManagementRef = ref();
const sampleSqlManagementRef = ref();

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
      chatId.value = "";
      chatRef.value?.init();
      chatHistoryRef.value?.init();
      break;
    case "chat_settings":
      chatSettingsRef.value?.loadChatConfig();
      break;
    case "data_source_management":
      dataSourceManagementRef.value?.init();
      break;
    case "llm_management":
      llmManagementRef.value?.init();
      break;
    case "sample_sql_management":
      sampleSqlManagementRef.value?.loadData();
      sampleSqlManagementRef.value?.loadConfig();
      break;
  }
};

/**
 * 切换对话
 */
const chatId = ref("");
const dataSourceId = ref("");
const dataSourceDisabled = ref(false);

const handleSwitchChat = (id: string) => {
  chatId.value = id;
  // 从会话列表中获取对应会话的数据源ID并设置
  const chat = chatHistoryRef.value?.chatHistoryList?.find(
    (item: any) => item.id === id,
  );
  if (chat) {
    dataSourceId.value = chat.dataSourceId;
  }
  dataSourceDisabled.value = true;
};

/**
 * 添加新对话
 */
const handleAddNewChat = () => {
  chatId.value = "";
  dataSourceId.value = "";
  dataSourceDisabled.value = false;
};

/**
 * 更新对话历史
 */
const handleUpdateChatHistory = (id: string) => {
  chatId.value = id;
  chatHistoryRef.value?.init(id);
};

/**
 * 更新数据源ID
 */
const handleUpdateDataSourceId = (id: string) => {
  dataSourceId.value = id;
};

export default defineComponent({
  components: {
    Chat,
    ChatSettings,
    ChatHistory,
    DataSourceManagement,
    LLMManagement,
    SampleSqlManagement,
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
      chatSettingsRef,
      chatHistoryRef,
      dataSourceManagementRef,
      chatId,
      handleSwitchChat,
      handleUpdateChatHistory,
      handleAddNewChat,
      dataSourceId,
      dataSourceDisabled,
      handleUpdateDataSourceId,
      llmManagementRef,
      sampleSqlManagementRef,
    };
  },
});
