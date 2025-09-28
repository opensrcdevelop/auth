<template>
  <div
    v-for="(message, index) in messages"
    :key="index"
    :class="[
      'message',
      message.role === 'USER' ? 'user-message' : 'assistant-message',
      message.role === 'USER' && index !== 0 ? 'user-message-mt' : '',
    ]"
  >
    <div
      :class="[
        'message-content',
        message.role === 'USER'
          ? 'user-message-content'
          : 'assistant-message-content',
      ]"
    >
      <LoadingMessage :message="message" />
      <TextMessage :message="message" />
      <MarkdownMessage :message="message" />
      <ChartMessage :message="message" />
      <TableMessage :message="message" />
      <HtmlReportMessage
        :message="message"
        @ready="handleAddHtmlReportIframe(message.questionId, $event)"
      />
      <DoneMessage
        :message="message"
        @resend-message="handleResendMessage"
        @analyze-data="handleAnalyzeData"
        @full-screen="handleFullscreen"
        @download-report="handleDownloadReport"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import ChartMessage from "./messages/ChartMessage.vue";
import DoneMessage from "./messages/DoneMessage.vue";
import HtmlReportMessage from "./messages/HtmlReportMessage.vue";
import LoadingMessage from "./messages/LoadingMessage.vue";
import TableMessage from "./messages/TableMessage.vue";
import TextMessage from "./messages/TextMessage.vue";
import MarkdownMessage from "./messages/MarkdownMessage.vue";

const props = withDefaults(
  defineProps<{
    messages: any[];
  }>(),
  {
    messages: () => [] as any[],
  }
);

const emits = defineEmits<{
  (e: "sendMessage", text: string): void;
  (
    e: "analyzeData",
    chartId: string,
    chatId: string,
    question: string,
    generateReport: boolean
  ): void;
}>();

const handleResendMessage = (question: string) => {
  emits("sendMessage", question);
};

const handleAnalyzeData = (
  chartId: string,
  chatId: string,
  question: string,
  generateReport: boolean
) => {
  emits("analyzeData", chartId, chatId, question, generateReport);
};

const htmlReportIFrame = new Map<string, HTMLIFrameElement>();

const handleAddHtmlReportIframe = (
  questionId: string,
  el: HTMLIFrameElement
) => {
  if (!el) return;
  htmlReportIFrame.set(questionId, el);
};

const handleFullscreen = (questionId: string) => {
  const el = htmlReportIFrame.get(questionId);
  if (!el) return;
  el.requestFullscreen();
};

const handleDownloadReport = (questionId: string) => {
  const el = htmlReportIFrame.get(questionId);
  if (!el) return;
  const iframeDoc = el.contentDocument || el.contentWindow?.document;

  if (iframeDoc) {
    const htmlContent = iframeDoc.documentElement.outerHTML;
    const blob = new Blob([htmlContent], { type: "text/html" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = iframeDoc.title || `report_${new Date().getTime()}`;
    document.body.appendChild(a);
    a.click();
    setTimeout(() => {
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    }, 0);
  }
};
</script>

<style lang="scss" scoped>
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
  margin-left: 0px;
}

.assistant-message-content {
  padding: 0;
  margin-bottom: 0;
}
</style>
