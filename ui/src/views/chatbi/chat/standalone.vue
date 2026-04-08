<script lang="ts">
import {computed, defineComponent, onMounted, onUnmounted, ref} from "vue";
import Chat from "../components/chat/Chat.vue";
import ChatHistory from "../components/chat/ChatHistory.vue";

// 判断是否是独立入口页面
const isStandalone = computed(() => {
  return window.location.pathname.includes("chatbi-chat.html");
});

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
      // 独立入口初始化
      chatRef.value?.init();
      chatHistoryRef.value?.init("");

      // 独立入口模式下设置全局样式
      if (isStandalone.value) {
        document.documentElement.style.height = "100%";
        document.body.style.height = "100%";
        document.body.style.margin = "0";
        document.body.style.padding = "0";
        document.body.style.overflow = "hidden";
      }
    });

    onUnmounted(() => {
      // 清理样式
      if (isStandalone.value) {
        document.documentElement.style.height = "";
        document.body.style.height = "";
        document.body.style.margin = "";
        document.body.style.padding = "";
        document.body.style.overflow = "";
      }
    });

    return {
      chatRef,
      chatHistoryRef,
      chatId,
      dataSourceId,
      isStandalone,
      handleSwitchChat,
      handleAddNewChat,
      handleUpdateChatHistory,
      handleUpdateDataSourceId,
    };
  },
});
</script>

<style lang="scss" scoped>
.chatbi-page {
  display: flex;
  flex-direction: column;
  height: 100%;

  // 独立入口模式
  &.standalone {
    height: 100vh;
    margin: 0;
    padding: 0;
    box-sizing: border-box;
  }

  .chat-container {
    display: flex;
    flex: 1;
    overflow: hidden;
  }
}

// 移动端适配
@media (max-width: 768px) {
  .chatbi-page.standalone {
    .chat-container {
      flex-direction: column;
    }
  }
}
</style>

<template>
  <div class="chatbi-page" :class="{ standalone: isStandalone }">
    <div class="chat-container">
      <ChatHistory
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