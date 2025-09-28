<template>
  <div class="chat-container">
    <div class="message-container" ref="messageContainer">
      <div class="empty-container" v-if="!messages.length">
        {{ greetingText }}
      </div>
      <ChatMessage
        :messages="messages"
        @send-message="sendMessage"
        @analyze-data="analyzeData"
      />
    </div>
    <div class="input-area">
      <a-textarea
        class="no-border-textarea"
        placeholder="请输入您的问题..."
        :auto-size="{
          minRows: 3,
          maxRows: 5,
        }"
        v-model="userInput"
        @keyup.enter.ctrl="sendMessage(userInput)"
      />
      <div class="bottom-bar">
        <div class="option">
          <a-space>
            <a-select
              placeholder="请选择数据源"
              size="mini"
              allow-search
              :bordered="false"
              v-model="selectedDataSource"
            >
              <a-option
                v-for="item in dataSourceList"
                :key="item.id"
                :value="item.id"
              >
                {{ item.name }}
              </a-option>
            </a-select>
            <a-select
              placeholder="请选择大模型"
              size="mini"
              allow-search
              :bordered="false"
              v-model="selectedModel"
            >
              <a-optgroup
                v-for="item in modelProviderList"
                :key="item.id"
                :label="item.name"
              >
                <a-option
                  v-for="(model, index) in item.optionModels"
                  :key="index"
                  :value="`${item.id}:${model}`"
                  >{{ model }}</a-option
                >
              </a-optgroup>
            </a-select>
          </a-space>
        </div>
        <div>
          <a-button
            type="primary"
            shape="circle"
            @click="sendMessage(userInput)"
            v-if="!loading"
          >
            <template #icon>
              <icon-arrow-up />
            </template>
          </a-button>
          <a-button
            type="primary"
            shape="circle"
            @click="stopGenerating()"
            v-else
          >
            <template #icon>
              <icon-record-stop />
            </template>
          </a-button>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import {useEventSource} from "@/hooks/useEventSource";
import {nextTick, reactive, ref, watch} from "vue";
import {generateRandomString, handleApiError, handleApiSuccess,} from "@/util/tool";
import {getEnabledDataSourceConf, getModelProviderList, getUserChatMessageHistory,} from "@/api/chatbi";
import {Message} from "@arco-design/web-vue";
import ChatMessage from "./components/ChatMessage.vue";

const props = withDefaults(
  defineProps<{
    chatId: string;
    dataSourceId: string;
  }>(),
  {
    chatId: undefined,
    dataSourceId: undefined,
  }
);

const emits = defineEmits<{
  (e: "updateChatHistory", id: string): void;
}>();

const { abort, fetchStream } = useEventSource();
const messageContainer = ref(null);
const messages = reactive([]);
const userInput = ref("");
const loading = ref(false);
const questionId = ref("");
const dataSourceList = reactive([]);
const modelProviderList = reactive([]);
const selectedDataSource = ref("");
const selectedModel = ref("");
const greetingText = ref("");
const activeChatId = ref("");

const init = () => {
  greetingText.value = greeting();
  // 获取已启用的数据源
  getEnabledDataSourceConf()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dataSourceList.length = 0;
        dataSourceList.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取已启用的数据源");
    });

  // 获取模型提供商列表
  getModelProviderList({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        modelProviderList.length = 0;
        modelProviderList.push(...data.list);

        if (modelProviderList.length > 0) {
          const firstProvider = modelProviderList[0];
          selectedModel.value = `${firstProvider.id}:${firstProvider.defaultModel}`;
        }
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取模型提供商列表");
    });
};

defineExpose({
  init,
});

watch(
  () => props.chatId,
  (newVal) => {
    // 切花
    if (newVal && newVal !== activeChatId.value) {
      activeChatId.value = newVal;
      handleGetChatMessageHistory(newVal);
    }

    // 开启新对话
    if (!newVal) {
      activeChatId.value = "";
      messages.length = 0;
    }
  }
);

watch(
  () => props.dataSourceId,
  (newVal) => {
    selectedDataSource.value = newVal;
  }
);

/**
 * 获取对话消息历史
 */
const handleGetChatMessageHistory = (chatId: string) => {
  getUserChatMessageHistory(chatId).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      messages.length = 0;
      data.forEach((item: any) => {
        handleMessage(item);
      });
    });
  });
};

/**
 * 问候语
 */
const greeting = () => {
  const hour = new Date().getHours();
  if (hour < 6) return "夜深了，注意休息！有什么我可以帮助你的吗？";
  if (hour < 9) return "早上好！有什么我可以帮助你的吗？";
  if (hour < 12) return "上午好！有什么我可以帮助你的吗？";
  if (hour < 14) return "中午好！有什么我可以帮助你的吗？";
  if (hour < 17) return "下午好！有什么我可以帮助你的吗？";
  if (hour < 19) return "傍晚好！有什么我可以帮助你的吗？";
  return "晚上好！有什么我可以帮助你的吗？";
};

/**
 * 发送消息
 */
const sendMessage = (input: string) => {
  if (!selectedDataSource.value) {
    Message.warning("请选择数据源");
    return;
  }

  if (!selectedModel.value) {
    Message.warning("请选择大模型");
    return;
  }

  const question = input || userInput.value;

  if (!question.trim() || loading.value) return;
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
      question,
      questionId: questionId.value,
      modelProviderId: selectedModel.value.split(":")[0],
      model: selectedModel.value.split(":")[1],
      dataSourceId: selectedDataSource.value,
      chatId: activeChatId.value,
    },
    onMessage: (message) => handleMessage(message),
    onError: (error) => {
      console.error(error);
      loading.value = false;
      const loadingItem = messages.find(
        (item) =>
          item.questionId === questionId.value && item.type === "LOADING"
      );
      if (loadingItem) {
        loadingItem.loading = false;
        loadingItem.error = true;
        loadingItem.content = "发生了未知错误";
      }
      abort();
    },
    onClose: () => {
      loading.value = false;
    },
  });

  nextTick(() => {
    scrollToBottom();
  });
};

/**
 * 数据分析
 */
const analyzeData = (
  chartId: string,
  chatId: string,
  question: string,
  generateReport: boolean
) => {
  if (!selectedModel.value) {
    Message.warning("请选择大模型");
    return;
  }

  questionId.value = generateRandomString(12);
  messages.push({
    role: "USER",
    type: "TEXT",
    content: "数据分析：" + question,
    questionId: questionId.value,
  });
  messages.push({
    role: "ASSISTANT",
    type: "LOADING",
    loading: true,
    content: "回答生成中...",
    questionId: questionId.value,
    error: false,
  });

  fetchStream({
    url: `/chatbi/analyze/stream`,
    body: {
      chatId,
      questionId: questionId.value,
      modelProviderId: selectedModel.value.split(":")[0],
      model: selectedModel.value.split(":")[1],
      chartId: chartId,
      generateReport,
    },
    onMessage: (message) => handleMessage(message),
    onError: (error) => {
      console.error(error);
      loading.value = false;
      const loadingItem = messages.find(
        (item) =>
          item.questionId === questionId.value && item.type === "LOADING"
      );
      if (loadingItem) {
        loadingItem.loading = false;
        loadingItem.error = true;
        loadingItem.content = "发生了未知错误";
      }
      abort();
    },
    onClose: () => {
      loading.value = false;
    },
  });

  nextTick(() => {
    scrollToBottom();
  });
};

/**
 * 重新发送消息
 */
const resendMessage = (qId: string) => {
  const userQuestion = messages.find(
    (item) => item.questionId === qId && item.role === "USER"
  );
  sendMessage(userQuestion.content);
};

/**
 * 处理消息
 */
const handleMessage = (message) => {
  activeChatId.value = message.chatId;
  // 如果当前对话为全新对话，则更新对话历史
  if (activeChatId.value !== props.chatId) {
    emits("updateChatHistory", activeChatId.value);
  }

  if (message.role === "USER") {
    messages.push(message);
    return;
  }

  if (message.type === "DONE") {
    loading.value = false;
    const loadingItem = messages.find(
      (item) =>
        item.questionId === message.questionId && item.type === "LOADING"
    );
    if (loadingItem) {
      loadingItem.loading = false;
      loadingItem.content = "回答完成";
    }
    messages.push({
      role: "assistant",
      type: message.type,
      questionId: message.questionId,
      chatId: message.chatId,
      chartId: message.chartId,
      actionType: message.actionType,
      rewrittenQuestion: message.rewrittenQuestion,
      time: message.time,
    });
    scrollToBottom();
    return;
  }

  if (message.type === "LOADING") {
    const loadingItem = messages.find(
      (item) =>
        item.questionId === message.questionId && item.type === "LOADING"
    );
    if (loadingItem) {
      loadingItem.content = message.content;
    } else {
      messages.push(message);
    }
    return;
  }

  if (message.type === "ERROR") {
    const loadingItem = messages.find(
      (item) =>
        item.questionId === message.questionId && item.type === "LOADING"
    );
    if (loadingItem) {
      loadingItem.loading = false;
      loadingItem.error = true;
      loadingItem.content = message.content;
    }
    return;
  }

  if (
    messages.length > 0 &&
    messages[messages.length - 1].role === "assistant"
  ) {
    const last = messages[messages.length - 1];

    // 类型相同，合并内容
    if (last.type === message.type) {
      if (["MARKDOWN", "TEXT", "HTML_REPORT"].includes(message.type)) {
        last.content += message.content;
      } else if (["ECHARTS", "TABLE"].includes(message.type)) {
        last.content = message.content;
      }
    } else {
      // 类型不同追加新消息
      messages.push({
        role: "assistant",
        type: message.type,
        content: message.content,
        questionId: message.questionId,
        chatId: message.chatId,
      });
    }
  } else {
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
const stopGenerating = (qId: string = questionId.value) => {
  abort();
  loading.value = false;
  if (messages.length > 0) {
    const loadingItem = messages.find(
      (item) => item.questionId === qId && item.type === "LOADING"
    );
    if (loadingItem) {
      loadingItem.loading = false;
      loadingItem.content = "回答已取消";
    }
  }
};

/**
 * 将消息容器滚动到底部
 */
const scrollToBottom = () => {
  nextTick(() => {
    if (messageContainer.value) {
      messageContainer.value.scrollTop = messageContainer.value.scrollHeight;
    }
  });
};
</script>

<style scoped lang="scss">
.chat-container {
  height: calc(100vh - 200px);
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  background: linear-gradient(135deg, #f5f7fa 0%, #e4e8f0 100%);
  border-radius: 12px;
  overflow-x: auto;

  /* WebKit 浏览器滚动条样式 */
  &::-webkit-scrollbar {
    width: 8px;
  }

  &::-webkit-scrollbar-track {
    background: transparent;
    border-radius: 3px;
  }

  /* 滚动条滑块 */
  &::-webkit-scrollbar-thumb {
    background: rgba(144, 147, 153, 0.3);
    border-radius: 3px;

    &:hover {
      background: rgba(144, 147, 153, 0.5);
    }
  }

  /* Firefox 滚动条样式 */
  scrollbar-width: thin;
  scrollbar-color: rgba(144, 147, 153, 0.3) transparent;

  &:hover {
    scrollbar-color: rgba(144, 147, 153, 0.5) transparent;
  }
}

.message-container {
  flex: 1;
  overflow-y: auto;
  padding: 20px;
  margin-bottom: 20px;

  /* WebKit 浏览器滚动条样式 */
  &::-webkit-scrollbar {
    width: 8px;
  }

  &::-webkit-scrollbar-track {
    background: transparent;
    border-radius: 3px;
  }

  /* 滚动条滑块 */
  &::-webkit-scrollbar-thumb {
    background: rgba(144, 147, 153, 0.3);
    border-radius: 3px;

    &:hover {
      background: rgba(144, 147, 153, 0.5);
    }
  }

  /* Firefox 滚动条样式 */
  scrollbar-width: thin;
  scrollbar-color: rgba(144, 147, 153, 0.3) transparent;

  &:hover {
    scrollbar-color: rgba(144, 147, 153, 0.5) transparent;
  }
}

.operator-container {
  margin-top: 8px;
}

.empty-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  width: 100%;
  height: 100%;
  padding: 20px;
  background-image: url('data:image/svg+xml;utf8,<svg t="1756736588621" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="1736" width="200" height="200"><path d="M239.445333 14.961778c-6.940444-19.911111-17.976889-19.911111-24.917333 0l-38.513778 112.753778c-6.656 19.911111-28.672 41.870222-48.924444 48.810666l-112.071111 38.115556c-20.024889 6.997333-20.024889 17.976889 0 24.917333l111.502222 38.684445c19.911111 6.997333 41.984 28.956444 48.924444 48.924444l39.082667 112.981333c6.940444 19.911111 17.976889 19.911111 24.917333 0l37.944889-112.412444c6.656-19.968 28.672-41.927111 48.64-48.924445l114.119111-38.968888c19.911111-6.940444 19.911111-17.92 0-24.860445L327.68 177.095111c-19.911111-6.599111-41.984-28.615111-48.924444-48.526222-0.284444 0.284444-39.367111-113.607111-39.367112-113.607111z" fill="%23fff" p-id="1737"></path><path d="M512 398.222222h56.888889v170.666667H512zM739.555556 398.222222h56.888888v170.666667h-56.888888z" fill="%23fff" p-id="1738"></path></svg>');
  background-repeat: no-repeat;
  background-position: bottom -100px right;
  background-size: 330px 330px;
  font-size: 28px;
  color: #6b5454;
  text-align: center;
}

.input-area {
  background-color: #ffffff;
  margin: 0 16px 12px 16px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.02);
  border-radius: 12px;
  padding: 6px;
  box-sizing: border-box;
  overflow: hidden;
  display: flex;
  flex-direction: column;
  gap: 2px;

  .no-border-textarea {
    border: none;
    background-color: #ffffff;

    &:focus {
      border: none;
      box-shadow: none;
    }

    &:hover {
      border: none;
    }

    :deep(.arco-textarea) {
      /* WebKit 浏览器滚动条样式 */
      &::-webkit-scrollbar {
        width: 6px;
      }

      &::-webkit-scrollbar-track {
        background: transparent;
        border-radius: 3px;
      }

      /* 滚动条滑块 */
      &::-webkit-scrollbar-thumb {
        background: rgba(144, 147, 153, 0.3);
        border-radius: 3px;

        &:hover {
          background: rgba(144, 147, 153, 0.5);
        }
      }

      /* Firefox 滚动条样式 */
      scrollbar-width: thin;
      scrollbar-color: rgba(144, 147, 153, 0.3) transparent;

      &:hover {
        scrollbar-color: rgba(144, 147, 153, 0.5) transparent;
      }
    }
  }

  .bottom-bar {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
}
</style>
