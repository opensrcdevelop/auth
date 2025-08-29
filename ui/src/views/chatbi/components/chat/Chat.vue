<template>
  <div class="chat-container">
    <div class="message-container" ref="messageContainer">
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
          <div
            v-if="message.type === 'ECHARTS'"
            class="echarts-container"
            :ref="(el) => initChart(el, message.content)"
          ></div>
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
        @keyup.enter.ctrl="sendMessage"
      />
      <div class="bottom-bar">
        <div class="option"></div>
        <div>
          <a-button
            type="primary"
            shape="circle"
            @click="sendMessage"
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
import {nextTick, onUnmounted, reactive, ref} from "vue";
import MarkdownIt from "markdown-it";
import hljs from "highlight.js";
import "highlight.js/styles/github.css";
import * as echarts from "echarts";
import {generateRandomString} from "@/util/tool";

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
    `<code class="inline-code">${md.utils.escapeHtml(
    token.content
  )}</code>`,
  `</div>`,
  ]
  .join("");
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
    // 销毁已存在的图表实例
    existingChart.dispose();
    // 移除对应的 observer
    const existingObserver = chartRefs.get(el + "_observer");
    if (existingObserver) {
      existingObserver.disconnect();
      chartRefs.delete(el + "_observer");
    }
  }

  // 延迟初始化确保 DOM 已完全渲染
  setTimeout(() => {
    const chart = echarts.init(el);
    chart.setOption(option);

    // 立即执行一次 resize
    chart.resize();

    const resizeObserver = new ResizeObserver(() => {
      chart.resize();
    });

    resizeObserver.observe(el);
    chartRefs.set(el, chart);
    chartRefs.set(el + "_observer", resizeObserver);
  }, 0);
};

/**
 * 销毁 ECharts 实例
 */
onUnmounted(() => {
  chartRefs.forEach((value, key) => {
    if (typeof key === "string" && key.endsWith("_observer")) {
      value.disconnect();
    } else {
      value.dispose();
    }
  });
  chartRefs.clear();
});

const { abort, fetchStream } = userEventSource();
const messageContainer = ref(null);
const messages = reactive([]);
const userInput = ref("");
const loading = ref(false);
const questionId = ref("");

/**
 * 发送消息
 */
const sendMessage = async () => {
  if (!userInput.value.trim() || loading.value) return;
  questionId.value = generateRandomString(12);
  messages.push({
    role: "user",
    type: "TEXT",
    content: userInput.value,
    questionId: questionId.value,
  });
  messages.push({
    role: "assistant",
    type: "LOADING",
    loading: true,
    content: "回答生成中...",
    questionId: questionId.value,
  });

  const question = userInput.value;
  userInput.value = "";
  loading.value = true;

  fetchStream({
    url: "/chatbi/chat/stream",
    body: {
      question,
      questionId: questionId.value,
      modelProviderId: "401572c0-828d-43e8-a497-d3e90d901e86",
      model: "qwen-coder-plus-latest",
      dataSourceId: "27058042-ae53-49cd-bd19-64a6e986f179",
    },
    onMessage: (message) => handleMessage(message),
    onError: (error) => {
      // loading.value = false;
      console.log(error);
      abort();
    },
    onClose: () => {
      loading.value = false;
    },
  });
};

/**
 * 处理消息
 */
const handleMessage = (message) => {
  const { questionId, chatId, type, content } = message;

  if (type === "DONE") {
    const loadingItem = messages.find(
      (item) => item.questionId === questionId && item.type === "LOADING"
    );
    if (loadingItem) {
      loadingItem.loading = false;
      loadingItem.content = "回答完成";
    }
    stopGenerating(questionId);
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
      } else if (type === "ECHARTS") {
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
    scrollToBottom();
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
    if (loadingItem) {
      loadingItem.loading = false;
      loadingItem.loadingMsg = "回答已取消";
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
    height: 330px;
  }
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
