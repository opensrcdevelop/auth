import {defineComponent, onMounted, ref} from "vue";
import Chat from "../components/chat/Chat.vue";
import ChatHistory from "../components/chat/ChatHistory.vue";
import {getChatConfig, updateChatConfig} from "@/api/chatbi";
import {Message} from "@arco-design/web-vue";

const chatRef = ref();
const chatHistoryRef = ref();
const chatId = ref("");
const dataSourceId = ref("");
const embedMode = ref(false);
const showSettings = ref(false);

const chatConfig = ref({
  maxSteps: 30,
  language: "简体中文",
  apiRetryCount: 3,
  apiTimeout: 300,
});

/**
 * 切换对话
 */
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
 * 更新数据源ID
 */
const handleUpdateDataSourceId = (id: string) => {
  dataSourceId.value = id;
};

/**
 * 加载对话配置
 */
const loadChatConfig = async () => {
  try {
    const res = await getChatConfig();
    if (res.data) {
      chatConfig.value = {...chatConfig.value, ...res.data};
    }
  } catch (e) {
    console.error("加载对话配置失败", e);
  }
};

/**
 * 保存对话配置
 */
const handleSaveConfig = async () => {
  try {
    await updateChatConfig(chatConfig.value);
    Message.success("保存成功");
    showSettings.value = false;
  } catch (e) {
    Message.error("保存失败");
  }
};

export default defineComponent({
  components: {
    Chat,
    ChatHistory,
  },
  setup() {
    onMounted(() => {
      // 检查是否为 iframe 嵌入模式
      embedMode.value = window.location.search.includes("embed=true");
      loadChatConfig();
    });

    return {
      chatRef,
      chatHistoryRef,
      chatId,
      dataSourceId,
      embedMode,
      showSettings,
      chatConfig,
      handleSwitchChat,
      handleAddNewChat,
      handleUpdateChatHistory,
      handleUpdateDataSourceId,
      handleSaveConfig,
    };
  },
});
