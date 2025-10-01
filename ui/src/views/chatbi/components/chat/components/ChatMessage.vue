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
      <HtmlReportMessage :message="message" />
      <DoneMessage :message="message" @resend-message="handleResendMessage" />
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
