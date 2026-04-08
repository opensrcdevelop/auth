<script setup lang="ts">
import {onMounted, reactive} from "vue";
import {getChatConfig, updateChatConfig} from "@/api/chatbi";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";

const chatConfigForm = reactive({
  maxThinkSteps: undefined,
  answerLanguage: undefined,
  llmApiRetryCount: undefined,
});

const chatConfigFormRules = {
  maxThinkSteps: [{ required: true, message: "最大思考步数未填写" }],
  answerLanguage: [{ required: true, message: "回答语言未填写" }],
  llmApiRetryCount: [{ required: true, message: "LLM API 重试次数未填写" }],
};

const loadChatConfig = () => {
  getChatConfig()
    .then((res: any) => {
      handleApiSuccess(res, (data: any) => {
        chatConfigForm.maxThinkSteps = data.maxThinkSteps;
        chatConfigForm.answerLanguage = data.answerLanguage;
        chatConfigForm.llmApiRetryCount = data.llmApiRetryCount;
      });
    })
    .catch((err: any) => handleApiError(err, "获取 ChatBI 对话配置"));
};

const handleSaveChatConfig = () => {
  updateChatConfig(chatConfigForm)
    .then((res: any) => {
      handleApiSuccess(res, () => {
        Notification.success("保存成功");
      });
    })
    .catch((err: any) => handleApiError(err, "保存 ChatBI 对话配置"));
};

onMounted(() => {
  loadChatConfig();
});

defineExpose({
  loadChatConfig,
});
</script>

<template>
  <div class="chat-settings">
    <div class="section-header">
      <div class="title">对话配置</div>
    </div>
    <a-form
      :model="chatConfigForm"
      :rules="chatConfigFormRules"
      layout="vertical"
      @submit-success="handleSaveChatConfig"
    >
      <a-row :gutter="24">
        <a-col :span="8">
          <a-form-item label="最大思考步数" field="maxThinkSteps">
            <a-input-number
              placeholder="请输入最大思考步数"
              v-model="chatConfigForm.maxThinkSteps"
              :min="30"
              :step="1"
            />
          </a-form-item>
        </a-col>
        <a-col :span="8">
          <a-form-item label="回答语言" field="answerLanguage">
            <a-input
              v-model="chatConfigForm.answerLanguage"
              placeholder="请输入回答语言，如：简体中文、English"
            />
          </a-form-item>
        </a-col>
        <a-col :span="8">
          <a-form-item label="LLM API 重试次数" field="llmApiRetryCount">
            <a-input-number
              placeholder="请输入 LLM API 重试次数"
              v-model="chatConfigForm.llmApiRetryCount"
              :min="3"
            />
          </a-form-item>
        </a-col>
      </a-row>
      <a-form-item hide-label>
        <a-button type="primary" html-type="submit">保存</a-button>
      </a-form-item>
    </a-form>
  </div>
</template>

<style scoped lang="scss">
.chat-settings {
  padding: 16px;
}

.section-header {
  margin-bottom: 16px;
}

.section-header .title {
  font-size: 20px;
  font-weight: 500;
  color: #1d2129;
}
</style>
