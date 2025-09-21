<template>
  <div class="chat-container">
    <div class="message-container" ref="messageContainer">
      <div class="empty-container" v-if="!messages.length">
        {{ greetingText }}
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
            <a-tag :color="message.error ? 'red' : 'arcoblue'">
              <template #icon>
                <icon-close-circle v-if="message.error" />
                <icon-loading v-if="!message.error && message.loading" />
                <icon-check-circle v-if="!message.error && !message.loading" />
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
          <div v-if="message.type === 'HTML_REPORT'">
            <div class="html-report-container">
              <div class="content">
                <iframe
                  :ref="(el) => addHtmlReportRef(message.questionId, el)"
                  :srcdoc="message.content"
                  frameborder="0"
                  width="100%"
                  height="100%"
                />
              </div>
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
                <a-divider v-if="message.chartId" direction="vertical" />
                <a-space v-if="message.chartId">
                  <a-tooltip content="喜欢" position="bottom" mini>
                    <a-button
                      size="mini"
                      shape="circle"
                      @click="handleVoteChart(message, 'LIKE')"
                    >
                      <template #icon>
                        <icon-thumb-up-fill
                          v-if="message?.feedback === 'LIKE'"
                        />
                        <icon-thumb-up v-else />
                      </template>
                    </a-button>
                  </a-tooltip>
                  <a-tooltip content="不喜欢" position="bottom" mini>
                    <a-button
                      size="mini"
                      shape="circle"
                      @click="handleVoteChart(message, 'DISLIKE')"
                    >
                      <template #icon>
                        <icon-thumb-down-fill
                          v-if="message?.feedback === 'DISLIKE'"
                        />
                        <icon-thumb-down v-else />
                      </template>
                    </a-button>
                  </a-tooltip>
                </a-space>
              </a-space>
            </div>
          </div>
          <div
            v-if="
              message.type === 'DONE' && message.actionType === 'ANALYZE_DATA'
            "
          >
            <div class="operator-container">
              <a-space>
                <a-button
                  type="text"
                  size="mini"
                  @click="toggleFullscreen(message.questionId)"
                >
                  <template #icon>
                    <icon-fullscreen />
                  </template>
                  <template #default>全屏显示</template>
                </a-button>
                <a-button
                  type="text"
                  size="mini"
                  @click="downloadAsPDF(message.questionId)"
                >
                  <template #icon>
                    <icon-download />
                  </template>
                  <template #default>下载</template>
                </a-button>
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
import {userEventSource} from "@/hooks/useEventSource";
import {nextTick, onUnmounted, reactive, ref} from "vue";
import MarkdownIt from "markdown-it";
import hljs from "highlight.js";
import "highlight.js/styles/github.css";
import * as echarts from "echarts";
import {generateRandomString, handleApiError, handleApiSuccess,} from "@/util/tool";
import {getEnabledDataSourceConf, getModelProviderList, voteChart,} from "@/api/chatbi";
import {Message} from "@arco-design/web-vue";
import html2canvas from "html2canvas";
import jsPDF from "jspdf";

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
const greetingText = ref("");

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
    error: false,
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
    error: false,
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
    loading.value = false;
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

    if (actionType === "ANALYZE_DATA") {
      if (!checkHtmlReportIframeReady(questionId)) {
        reloadHtmlReportIframe(questionId);
      }
    }
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

  if (type === "ERROR") {
    const loadingItem = messages.find(
      (item) => item.questionId === questionId && item.type === "LOADING"
    );
    if (loadingItem) {
      loadingItem.loading = false;
      loadingItem.error = true;
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
      if (["MARKDOWN", "TEXT", "HTML_REPORT"].includes(type)) {
        last.content += content;
      } else if (["ECHARTS", "TABLE"].includes(type)) {
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
      doneMessage.feedback =
        doneMessage?.feedback === feedback ? undefined : feedback;
    });
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

/**
 * HTML Report
 */
const htmlReportRefs = new Map<string, HTMLElement>();
const addHtmlReportRef = (qId, el) => {
  if (!el) return;
  htmlReportRefs.set(qId, el);
};

/**
 * 检查 iframe 是否加载完成
 */
const checkHtmlReportIframeReady = (qId: string) => {
  const el = htmlReportRefs.get(qId);
  if (!el) return false;
  const iframeWindow = (el as HTMLIFrameElement).contentWindow;
  try {
    if (iframeWindow.document.readyState === "complete") {
      return true;
    }
    return false;
  } catch (e) {
    console.error("检查 iframe 是否加载完成失败:", e);
    return false;
  }
};

/**
 * 重新加载 iframe
 */
const reloadHtmlReportIframe = (qId: string) => {
  const el = htmlReportRefs.get(qId);
  if (!el) return;
  (el as HTMLIFrameElement).src = (el as HTMLIFrameElement).src;
}

/**
 * 切换全屏
 */
const toggleFullscreen = (qId: string) => {
  const el = htmlReportRefs.get(qId);
  if (!el) return;

  if (el?.requestFullscreen) {
    el.requestFullscreen();
  }
};

/**
 * 下载为 PDF
 */
const downloadAsPDF = async (qId: string) => {
  const el = htmlReportRefs.get(qId);
  if (!el) return;

  try {
    const document =
      (el as HTMLIFrameElement).contentDocument ||
      (el as HTMLIFrameElement).contentWindow?.document;
    if (!document) {
      console.error("无法获取 iframe 的 document 对象");
      return;
    }
    const title = document.title || `report_${new Date().getTime()}`;
    const canvas = await html2canvas(document.body, {
      scale: 2,
      useCORS: true,
      allowTaint: true,
      logging: false,
      width: document.body.scrollWidth,
      height: document.body.scrollHeight,
    });

    const imgData = canvas.toDataURL("image/png");
    const pdf = new jsPDF({
      orientation: canvas.width > canvas.height ? "landscape" : "portrait",
      unit: "px",
      format: [canvas.width, canvas.height],
    });
    pdf.addImage(imgData, "PNG", 0, 0, canvas.width, canvas.height);
    pdf.save(`${title}.pdf`);
  } catch (error) {
    console.error("下载PDF失败:", error);
  }
};
</script>

<style lang="scss" scoped>
@use "./style.scss";
</style>
