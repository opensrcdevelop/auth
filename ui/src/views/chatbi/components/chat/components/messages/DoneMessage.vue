<template>
  <div v-if="message.type === 'DONE'">
    <div class="operator-container">
      <a-space>
        <a-button type="text" size="mini" @click="handleResendMessage">
          <template #icon>
            <icon-refresh />
          </template>
          <template #default>重新生成</template>
        </a-button>
        <a-button
          type="text"
          size="mini"
          v-if="message.answerId"
          @click="handleGetAnsweredSql(message)"
        >
          <template #icon>
            <icon-copy />
          </template>
          <template #default>复制 SQL</template>
        </a-button>
        <a-divider v-if="message.answerId" direction="vertical" />
        <a-space v-if="message.answerId">
          <a-tooltip content="喜欢" position="bottom" mini>
            <a-button
              size="mini"
              shape="circle"
              @click="handleVoteAnswer(message, 'LIKE')"
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
              @click="handleVoteAnswer(message, 'DISLIKE')"
            >
              <template #icon>
                <icon-thumb-down-fill v-if="message?.feedback === 'DISLIKE'" />
                <icon-thumb-down v-else />
              </template>
            </a-button>
          </a-tooltip>
        </a-space>
      </a-space>
      <a-space>
        <span class="time">{{ message.time }}</span>
      </a-space>
    </div>
  </div>
</template>

<script setup lang="ts">
import {getAnsweredSql, voteAnswer} from "@/api/chatbi";
import {copyToClipboard, handleApiError, handleApiSuccess} from "@/util/tool";
import {Message} from "@arco-design/web-vue";

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
}>();

const handleResendMessage = () => {
  emits("resendMessage", props.message.rewrittenQuestion);
};

const handleVoteAnswer = (doneMessage: any, feedback: string) => {
  const { answerId } = doneMessage;
  voteAnswer({
    answerId,
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

const handleGetAnsweredSql = (doneMessage: any) => {
  getAnsweredSql(doneMessage.answerId).then((result: any) => {
    handleApiSuccess(result, async (data: any) => {
      if (data.sql) {
        const result = await copyToClipboard(data.sql);
        if (result) {
          Message.success("复制成功");
        } else {
          Message.error("复制失败");
        }
      } else {
        Message.warning("该回答没有生成 SQL");
      }
    });
  }).catch((err: any) => {
    handleApiError(err, "获取回答的 SQL")
  })
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
