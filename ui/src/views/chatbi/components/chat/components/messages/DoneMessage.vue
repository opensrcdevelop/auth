<template>
  <div
    v-if="message.type === 'DONE' && message.actionType === 'GENERATE_CHART'"
  >
    <div class="operator-container">
      <a-space>
        <a-button type="text" size="mini" @click="handleResendMessage">
          <template #icon>
            <icon-refresh />
          </template>
          <template #default>重新生成</template>
        </a-button>
        <a-button
          v-if="message.chartId"
          type="text"
          size="mini"
          @click="handleAnalyzeData"
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
                <icon-thumb-up-fill v-if="message?.feedback === 'LIKE'" />
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
                <icon-thumb-down-fill v-if="message?.feedback === 'DISLIKE'" />
                <icon-thumb-down v-else />
              </template>
            </a-button>
          </a-tooltip>
        </a-space>
      </a-space>
    </div>
  </div>
  <div
    v-if="message.type === 'DONE' && message.actionType === 'GENERATE_REPORT'"
  >
    <div class="operator-container">
      <a-space>
        <a-button type="text" size="mini" @click="handleFullscreen">
          <template #icon>
            <icon-fullscreen />
          </template>
          <template #default>全屏显示</template>
        </a-button>
        <a-button type="text" size="mini" @click="handleDownloadReport">
          <template #icon>
            <icon-download />
          </template>
          <template #default>下载</template>
        </a-button>
      </a-space>
    </div>
  </div>
</template>

<script setup lang="ts">
import {voteChart} from "@/api/chatbi";
import {handleApiError, handleApiSuccess} from "@/util/tool";

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const emits = defineEmits<{
  (e: "resendMessage", questionId: string): void;
  (e: "analyzeData", chartId: string, chatId: string, questionId: string): void;
  (e: "fullScreen", questionId: string): void;
  (e: "downloadReport", questionId: string): void;
}>();

const handleResendMessage = () => {
  emits("resendMessage", props.message.questionId);
};

const handleAnalyzeData = () => {
  emits(
    "analyzeData",
    props.message.chartId,
    props.message.chatId,
    props.message.questionId
  );
};

const handleFullscreen = () => {
  emits("fullScreen", props.message.questionId);
};

const handleDownloadReport = () => {
  emits("downloadReport", props.message.questionId);
};

const handleVoteChart = (doneMessage: any, feedback: string) => {
  const { chartId } = doneMessage;
  voteChart({
    chartId,
    feedback: doneMessage?.feedback === feedback ? undefined : feedback,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        doneMessage.feedback =
          doneMessage?.feedback === feedback ? undefined : feedback;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "反馈图表");
    });
};
</script>

<style scoped lang="scss"></style>
