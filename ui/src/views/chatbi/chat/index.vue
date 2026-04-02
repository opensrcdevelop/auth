<script lang="ts">
import {defineComponent, onMounted, ref} from "vue";
import Chat from "../components/chat/Chat.vue";
import ChatHistory from "../components/chat/ChatHistory.vue";

const chatRef = ref();
const chatHistoryRef = ref();
const chatId = ref("");
const dataSourceId = ref("");
const embedMode = ref(false);

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
      embedMode.value = window.location.search.includes("embed=true");
    });

    return {
      chatRef,
      chatHistoryRef,
      chatId,
      dataSourceId,
      embedMode,
      handleSwitchChat,
      handleAddNewChat,
      handleUpdateChatHistory,
      handleUpdateDataSourceId,
    };
  },
});
</script>

<style lang="scss" scoped>
.chat-page {
  display: flex;
  flex-direction: column;
  height: 100%;

  &.embed-mode {
    height: 100vh;
    padding: 0;
  }

  .chat-container {
    display: flex;
    flex: 1;
    overflow: hidden;

    &.full-screen {
      height: 100%;
    }
  }
}
</style>

<template>
  <div class="chat-page" :class="{ 'embed-mode': embedMode }">
    <div class="chat-container" :class="{ 'full-screen': embedMode }">
      <ChatHistory
        v-if="!embedMode"
        ref="chatHistoryRef"
        @switchChat="handleSwitchChat"
        @addNewChat="handleAddNewChat"
        @updateDataSourceId="handleUpdateDataSourceId"
      />
      <Chat
        ref="chatRef"
        :chatId="chatId"
        :dataSourceId="dataSourceId"
        @updateChatHistory="handleUpdateChatHistory"
      />
    </div>
  </div>
</template>
