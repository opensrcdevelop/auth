<template>
  <div v-if="message.type === 'DONE'">
    <div class="operator-container">
      <a-space v-if="message.actionType === 'GENERATE_CHART'">
        <a-button type="text" size="mini" @click="handleResendMessage">
          <template #icon>
            <icon-refresh />
          </template>
          <template #default>重新生成</template>
        </a-button>
        <a-popconfirm
          content="是否需要生成分析报告（此操作耗时较长）？"
          okText="需要"
          cancelText="不需要"
          position="rt"
          @ok="handleAnalyzeData(true)"
          @cancel="handleAnalyzeData(false)"
        >
          <a-button v-if="message.chartId" type="text" size="mini">
            <template #icon size="mini">
              <icon-computer />
            </template>
            <template #default>数据分析</template>
          </a-button>
        </a-popconfirm>
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
      <a-space v-if="message.actionType === 'GENERATE_REPORT'">
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
      <a-space>
        <span class="time">{{ message.time }}</span>
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
  (
    e: "analyzeData",
    chartId: string,
    chatId: string,
    question: string,
    generateReport: boolean
  ): void;
  (e: "fullScreen", questionId: string): void;
  (e: "downloadReport", questionId: string): void;
}>();

const handleResendMessage = () => {
  console.log(props.message)
  emits("resendMessage", props.message.rewrittenQuestion);
};

const handleAnalyzeData = (generateReport: boolean) => {
  emits(
    "analyzeData",
    props.message.chartId,
    props.message.chatId,
    props.message.rewrittenQuestion,
    generateReport
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

<style scoped lang="scss">
.operator-container {
  display: flex;
  margin-top: 8px;
  justify-content: space-between;
  align-items: center;

  .time {
    font-size: 12px;
    color: var(--color-text-3);
  }
}
</style>
