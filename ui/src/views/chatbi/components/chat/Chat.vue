<template>
  <div class="chat-container">
    <div class="message-container" ref="messageContainer">
      <div class="empty-container" v-if="!messages.length">
        {{ greeting() }}
      </div>
      <div
        v-for="(message, index) in messages"
        :key="index"
        :class="[
          'message',
          message.role === 'user' ? 'user-message' : 'assistant-message',
          message.role === 'user' && index !== 0 ? 'user-message-mt' : '',
        ]"
      >
        <div
          :class="[
            'message-content',
            message.role === 'user'
              ? 'user-message-content'
              : 'assistant-message-content',
          ]"
        >
          <div v-if="message.type === 'LOADING'" class="avatar-container">
            <a-avatar class="avatar-assistant">
              <icon-font type="icon-assistant"></icon-font>
            </a-avatar>
            <a-tag color="arcoblue">
              <template #icon>
                <icon-loading v-if="message.loading" />
                <icon-check-circle v-else />
              </template>
              {{ message.content }}
            </a-tag>
          </div>
          <div v-if="message.type === 'LOADING'"></div>
          <div v-if="message.type === 'TEXT'" v-text="message.content"></div>
          <div
            v-if="message.type === 'MARKDOWN'"
            v-html="renderMarkdown(message.content)"
            class="markdown-body"
          ></div>
          <div v-if="message.type === 'CHART'" class="echarts-container">
            <div class="title">
              {{ message.content.title.text }}
            </div>
            <div class="description">
              {{ message.content.title.description }}
            </div>
            <div
              class="chart"
              :ref="(el) => initChart(el, message.content.option)"
            ></div>
          </div>
          <div v-if="message.type === 'TABLE'">
            <div class="table-container">
              <div class="title">
                {{ message.content.title.text }}
              </div>
              <div class="description">
                {{ message.content.title.description }}
              </div>
              <a-table
                column-resizable
                stripe
                :columns="message.content.columns"
                :data="message.content.data"
              />
            </div>
          </div>
          <div
            v-if="
              message.type === 'DONE' && message.actionType === 'GENERATE_CHART'
            "
          >
            <div class="operator-container">
              <a-space>
                <a-button
                  type="text"
                  size="mini"
                  @click="resendMessage(message.questionId)"
                >
                  <template #icon>
                    <icon-refresh />
                  </template>
                  <template #default>重新生成</template>
                </a-button>
                <a-button
                  v-if="message.chartId"
                  type="text"
                  size="mini"
                  @click="
                    analyzeData(
                      message.chartId,
                      message.chatId,
                      message.questionId
                    )
                  "
                >
                  <template #icon size="mini">
                    <icon-computer />
                  </template>
                  <template #default>数据分析</template>
                </a-button>
                <a-divider direction="vertical" />
                <a-space v-if="message.chartId">
                  <a-tooltip content="喜欢" position="bottom" mini>
                    <a-button size="mini" shape="circle" @click="handleVoteChart(message, 'LIKE')">
                      <template #icon>
                        <icon-thumb-up-fill v-if="message?.feedback === 'LIKE'" />
                        <icon-thumb-up v-else />
                      </template>
                    </a-button>
                  </a-tooltip>
                  <a-tooltip content="不喜欢" position="bottom" mini>
                    <a-button size="mini" shape="circle" @click="handleVoteChart(message, 'DISLIKE')">
                      <template #icon>
                        <icon-thumb-down-fill v-if="message?.feedback === 'DISLIKE'" />
                        <icon-thumb-down v-else />
                      </template>
                    </a-button>
                  </a-tooltip>
                </a-space>
              </a-space>
            </div>
          </div>
        </div>
      </div>
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
                >{{ item.name }}</a-option
              >
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
import {userEventSource} from "@/hooks/useEventSource";
import {nextTick, onMounted, onUnmounted, reactive, ref} from "vue";
import MarkdownIt from "markdown-it";
import hljs from "highlight.js";
import "highlight.js/styles/github.css";
import * as echarts from "echarts";
import {generateRandomString, handleApiError, handleApiSuccess,} from "@/util/tool";
import {getDataSourceConfList, getModelProviderList, voteChart} from "@/api/chatbi";
import {Message} from "@arco-design/web-vue";

const { abort, fetchStream } = userEventSource();
const messageContainer = ref(null);
const messages = reactive([]);
const userInput = ref("");
const loading = ref(false);
const questionId = ref("");
const dataSourceList = reactive([]);
const modelProviderList = reactive([]);
const selectedDataSource = ref("");
const selectedModel = ref("");

onMounted(() => {
  // 获取数据源列表
  getDataSourceConfList({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dataSourceList.push(...data.list);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取数据源列表");
    });

  // 获取模型提供商列表
  getModelProviderList({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
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
});

onUnmounted(() => {
  // 销毁 ECharts 实例
  chartRefs.forEach((value, key) => {
    if (typeof key === "string" && key.endsWith("_observer")) {
      value.disconnect();
    } else {
      value.dispose();
    }
  });
  chartRefs.clear();
});

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
    role: "user",
    type: "TEXT",
    content: question,
    questionId: questionId.value,
  });
  messages.push({
    role: "assistant",
    type: "LOADING",
    loading: true,
    content: "回答生成中...",
    questionId: questionId.value,
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
    },
    onMessage: (message) => handleMessage(message),
    onError: (error) => {
      loading.value = false;
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
const analyzeData = (chartId: string, chatId: string, qId: string) => {
  // 获取用户提问
  const userQuestion = messages.find(
    (item) => item.questionId === qId && item.role === "user"
  );
  if (!userQuestion) return;

  if (!selectedModel.value) {
    Message.warning("请选择大模型");
    return;
  }

  questionId.value = generateRandomString(12);
  messages.push({
    role: "user",
    type: "TEXT",
    content: "数据分析：" + userQuestion.content,
    questionId: questionId.value,
  });
  messages.push({
    role: "assistant",
    type: "LOADING",
    loading: true,
    content: "回答生成中...",
    questionId: questionId.value,
  });

  fetchStream({
    url: "/chatbi/analyze/stream",
    body: {
      chatId: chatId,
      questionId: questionId.value,
      modelProviderId: selectedModel.value.split(":")[0],
      model: selectedModel.value.split(":")[1],
      chartId: chartId,
    },
    onMessage: (message) => handleMessage(message),
    onError: (error) => {
      loading.value = false;
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
    (item) => item.questionId === qId && item.role === "user"
  );
  sendMessage(userQuestion.content);
};

/**
 * 处理消息
 */
const handleMessage = (message) => {
  const { actionType, chartId, questionId, chatId, type, content } = message;

  if (type === "DONE") {
    const loadingItem = messages.find(
      (item) => item.questionId === questionId && item.type === "LOADING"
    );
    if (loadingItem) {
      loadingItem.loading = false;
      loadingItem.content = "回答完成";
    }
    messages.push({
      role: "assistant",
      type,
      questionId,
      chatId,
      chartId,
      actionType,
    });
    stopGenerating();
    scrollToBottom();
    return;
  }

  if (type === "LOADING") {
    const loadingItem = messages.find(
      (item) => item.questionId === questionId && item.type === "LOADING"
    );
    if (loadingItem) {
      loadingItem.content = content;
    }
    return;
  }

  if (
    messages.length > 0 &&
    messages[messages.length - 1].role === "assistant"
  ) {
    const last = messages[messages.length - 1];

    // 类型相同，合并内容
    if (last.type === type) {
      if (type === "MARKDOWN" || type === "TEXT") {
        last.content += content;
      } else if (type === "ECHARTS" || type === "TABLE") {
        last.content = content;
      }
    } else {
      // 类型不同追加新消息
      messages.push({
        role: "assistant",
        type,
        content,
        questionId,
        chatId,
      });
    }
  } else {
    messages.push({
      role: "assistant",
      type,
      content,
      questionId,
      chatId,
    });
  }
  nextTick(() => {
    if (type === "MARKDOWN") {
      // 添加复制按钮事件监听
      const copyButtons = document.querySelectorAll(".copy-code-button");
      copyButtons.forEach((button) => {
        button.addEventListener("click", () => {
          const code = decodeURIComponent(button.getAttribute("data-code"));

          if (navigator.clipboard && window.isSecureContext) {
            navigator.clipboard
              .writeText(code)
              .then(() => {
                button.textContent = "已复制";
                setTimeout(() => {
                  button.textContent = "复制";
                }, 2000);
              })
              .catch((err) => {
                console.error("复制失败:", err);
              });
          } else {
            fallbackCopyTextToClipboard(code, button);
          }
        });
      });

      // 添加折叠按钮事件监听
      const foldButtons = document.querySelectorAll(".fold-code-button");
      foldButtons.forEach((button) => {
        button.addEventListener("click", () => {
          const container = button.closest(".code-block-container");
          const content = container.querySelector(
            ".code-block-content"
          ) as HTMLElement;
          // 切换折叠类
          content.classList.toggle("folded");

          // 更新按钮文本
          if (content.classList.contains("folded")) {
            button.textContent = "展开";
          } else {
            button.textContent = "折叠";
          }
        });
      });
    }
  });
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
    if (loadingItem && loadingItem.loading) {
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

/**
 * 投票图表
 */
const handleVoteChart = (doneMessage: any, feedback: string) => {
  const { chartId } = doneMessage;
  voteChart({
    chartId,
    feedback: doneMessage?.feedback === feedback ? undefined : feedback,
  }).then((result: any) => {
    handleApiSuccess(result, () => {
      doneMessage.feedback = doneMessage?.feedback === feedback ? undefined : feedback;;
    })
  })
};

/**
 * 复制文本到剪切板
 */
const fallbackCopyTextToClipboard = (text: string, button: Element) => {
  const textArea = document.createElement("textarea");
  textArea.value = text;
  textArea.style.position = "fixed";
  textArea.style.left = "-999999px";
  textArea.style.top = "-999999px";
  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();

  try {
    const successful = document.execCommand("copy");
    if (successful) {
      button.textContent = "已复制";
      setTimeout(() => {
        button.textContent = "复制";
      }, 2000);
    } else {
      console.error("复制失败");
    }
  } catch (err) {
    console.error("复制失败:", err);
  }

  document.body.removeChild(textArea);
};

/**
 * Markdown
 */
const md = new MarkdownIt({
  html: true,
  linkify: true,
  typographer: true,
  highlight: (str, lang) => {
    if (lang && hljs.getLanguage(lang)) {
      try {
        const highlightedCode = hljs.highlight(str, { language: lang }).value;
        return [
          `<div class="code-block-container">`,
          `<div class="code-block-header">`,
          `<span class="code-language">${lang}</span>`,
          `<div class="code-block-buttons">`,
          `<button class="copy-code-button" data-code="${encodeURIComponent(
            str
          )}">复制</button>`,
          `<button class="fold-code-button">折叠</button>`,
          `</div>`,
          `</div>`,
          `<div class="code-block-content">`,
          `<pre class="hljs"><code>${highlightedCode}</code></pre>`,
          `</div>`,
          `</div>`,
        ].join("");
      } catch (_) {}
    }
    return [
      `<div class="code-block-container">`,
      `<div class="code-block-header">`,
      `<span class="code-language">text</span>`,
      `<button class="copy-code-button" data-code="${encodeURIComponent(
        str
      )}">复制</button>`,
      `</div>`,
      `<pre class="hljs"><code>${md.utils.escapeHtml(str)}</code></pre>`,
      `</div>`,
    ].join("");
  },
});
// 添加行内代码的渲染规则
md.renderer.rules.code_inline = (tokens, idx) => {
  const token = tokens[idx];
  return [
    `<div class="inline-code-container">`,
    `<code class="inline-code">${md.utils.escapeHtml(token.content)}</code>`,
    `</div>`,
  ].join("");
};
const renderMarkdown = (text) => md.render(text);

/**
 * Echarts
 */
const chartRefs = new Map();
const initChart = (el, option) => {
  if (!el) return;

  // 检查是否已存在图表实例
  const existingChart = chartRefs.get(el);
  if (existingChart) {
    const existingOption = existingChart.getOption();
    const isOptionChanged =
      JSON.stringify(existingOption) !== JSON.stringify(option);

    if (!isOptionChanged) {
      // 选项未变化，不重新渲染
      return;
    }

    // 选项变化，更新图表
    existingChart.setOption(option, true);
    return;
  }

  // 延迟初始化确保 DOM 已完全渲染
  setTimeout(() => {
    const chart = echarts.init(el);
    chart.setOption(option);

    chart.resize();

    const resizeObserver = new ResizeObserver(() => {
      chart.resize();
    });

    resizeObserver.observe(el);
    chartRefs.set(el, chart);
    chartRefs.set(el + "_observer", resizeObserver);
  }, 0);
};
</script>

<style lang="scss" scoped>
.chat-container {
  height: calc(100vh - 200px);
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  background: linear-gradient(135deg, #f5f7fa 0%, #e4e8f0 100%);
  border-radius: 12px;
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

  .avatar-container {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .avatar-assistant {
    background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
    color: "white";
    margin-bottom: 16px;
  }

  .message {
    margin-bottom: 20px;
    display: flex;
    align-items: flex-start;
  }

  .user-message {
    flex-direction: row-reverse;
  }

  .user-message-mt {
    margin-top: 20px;
  }

  .assistant-message {
    margin-bottom: 0;
  }

  .message-content {
    width: 100%;
    padding: 12px 16px;
    border-radius: 8px;
    margin-left: 12px;
    line-height: 1.6;
    font-size: 14px;
    color: #2c3e50;
  }

  .user-message-content {
    width: fit-content;
    background-color: #ecf5ff;
    margin-right: 12px;
    margin-left: 0px;
  }

  .assistant-message-content {
    padding: 0;
    margin-bottom: 0;
  }

  .echarts-container {
    background-color: #fff;
    border-radius: 8px;
    margin-top: 4px;
    width: 100%;

    .title {
      font-size: 16px;
      font-weight: 700;
      color: 1d2129;
      padding: 12px 12px 4px 12px;
    }

    .description {
      font-size: 12px;
      color: #86909c;
      font-weight: 400;
      padding: 0 12px;
    }

    .chart {
      height: 330px;
      width: 100%;
    }
  }

  .table-container {
    background-color: #fff;
    border-radius: 8px;
    margin-top: 4px;
    padding: 0 16px 16px 12px;
    width: 100%;

    .title {
      font-size: 16px;
      font-weight: 700;
      color: 1d2129;
      padding: 12px 12px 4px 0;
    }

    .description {
      font-size: 12px;
      color: #86909c;
      font-weight: 400;
      padding: 0 12px 12px 0;
    }
  }

  .operator-container {
    margin-top: 8px;
  }
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

:deep(.markdown-body) {
  p,
  pre {
    margin-bottom: 0;
  }

  ul {
    padding-left: 12px;
  }

  /** 代码块容器 */
  .code-block-container {
    position: relative;
    margin: 4px 0;
    border-radius: 8px;
    background: #fff;
  }

  /** 代码块头部 */
  .code-block-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 8px 12px;
    background: #f6f8fa;
    border-top-left-radius: 8px;
    border-top-right-radius: 8px;
  }

  /** 代码块内容 */
  .code-block-content {
    font-family: Consolas, Monaco, "Andale Mono", "Ubuntu Mono", monospace;
    font-size: 14px;
    overflow-y: auto;
    transition: max-height 0.3s ease-out, opacity 0.3s ease-out;
    max-height: 1000px;
    opacity: 1;
  }

  /** 折叠状态 */
  .code-block-content.folded {
    max-height: 0;
    opacity: 0;
  }

  /** 代码语言标签 */
  .code-language {
    font-size: 12px;
    color: #666;
    text-transform: uppercase;
  }

  /** 代码块按钮组 */
  .code-block-buttons {
    display: flex;
    gap: 8px;
  }

  /** 复制按钮 */
  .copy-code-button {
    padding: 2px 8px;
    font-size: 12px;
    background: transparent;
    border: 1px solid #ddd;
    border-radius: 4px;
    cursor: pointer;
    color: #666;
    transition: all 0.2s;

    &:hover {
      background: #f0f0f0;
      color: #333;
    }
  }

  /** 折叠按钮 */
  .fold-code-button {
    padding: 2px 8px;
    font-size: 12px;
    background: transparent;
    border: 1px solid #ddd;
    border-radius: 4px;
    cursor: pointer;
    color: #666;
    transition: all 0.2s;

    &:hover {
      background: #f0f0f0;
      color: #333;
    }
  }

  /** 代码块 */
  pre.hljs {
    margin: 0;
    padding: 12px;
    background: transparent;
    overflow-x: auto;

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

  /** 行内代码 */
  .inline-code-container {
    margin: 4px 0;
  }

  .inline-code {
    padding: 2px 6px;
    font-size: 0.9em;
    color: #e83e8c;
    background-color: rgba(27, 31, 35, 0.05);
    border-radius: 3px;
    font-family: Consolas, Monaco, "Andale Mono", "Ubuntu Mono", monospace;
  }
}
</style>
