import {defineComponent, onMounted, ref} from "vue";
import Chat from "../components/chat/Chat.vue";
import ChatHistory from "../components/chat/ChatHistory.vue";

const chatRef = ref();
const chatHistoryRef = ref();
const chatId = ref("");
const dataSourceId = ref("");

const handleSwitchChat = (id: string) => {
  chatId.value = id;
};

const handleAddNewChat = () => {
  chatId.value = "";
  dataSourceId.value = "";
};

const handleUpdateChatHistory = (id: string) => {
  chatId.value = id;
  chatHistoryRef.value?.init(id);
};

const handleUpdateDataSourceId = (id: string) => {
  dataSourceId.value = id;
};

export default defineComponent({
  components: {
    Chat,
    ChatHistory,
  },
  setup() {
    onMounted(() => {
      chatRef.value?.init();
      chatHistoryRef.value?.init();
    });

    return {
      chatRef,
      chatHistoryRef,
      chatId,
      dataSourceId,
      handleSwitchChat,
      handleAddNewChat,
      handleUpdateChatHistory,
      handleUpdateDataSourceId,
    };
  },
});
