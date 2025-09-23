<template>
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
      <LoadingMessage :message="message" />
      <TextMessage :message="message" />
      <MarkdownMessage :message="message" />
      <ChartMessage :message="message" />
      <TableMessage :message="message" />
      <HtmlReportMessage
        :message="message"
        :ref="(el) => addHtmlReportMessageRef(message.questionId, el)"
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
  (e: "analyzeData", chartId: string, chatId: string, question: string): void;
}>();

const htmlReportMessageRefs = new Map<any, any>();

const addHtmlReportMessageRef = (questionId, el) => {
  if (!el) return;
  htmlReportMessageRefs.set(questionId, el);
};

const handleResendMessage = (questionId: string) => {
  const userQuestion = props.messages.find(
    (item) => item.questionId === questionId && item.role === "user"
  );
  if (!userQuestion) return;
  emits("sendMessage", userQuestion.content);
};

const handleAnalyzeData = (
  chartId: string,
  chatId: string,
  questionId: string
) => {
  const userQuestion = props.messages.find(
    (item) => item.questionId === questionId && item.role === "user"
  );
  if (!userQuestion) return;
  emits("analyzeData", chartId, chatId, userQuestion.content);
};

const handleFullscreen = (questionId: string) => {
  const htmlReportMessageRef = htmlReportMessageRefs.get(questionId);
  if (!htmlReportMessageRef) return;
  console.log(111);
  htmlReportMessageRef.toFullscreen();
};

const handleDownloadReport = (questionId: string) => {
  const htmlReportMessageRef = htmlReportMessageRefs.get(questionId);
  if (!htmlReportMessageRef) return;
  htmlReportMessageRef.downloadAsPdf();
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
