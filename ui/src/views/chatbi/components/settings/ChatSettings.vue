<script lang="ts">
import {defineComponent, onMounted, ref} from "vue";
import {getChatConfig, updateChatConfig} from "@/api/chatbi";
import {Message} from "@arco-design/web-vue";

const chatConfig = ref({
  maxSteps: 30,
  language: "简体中文",
  apiRetryCount: 3,
});

const loading = ref(false);

const loadChatConfig = async () => {
  try {
    const res = await getChatConfig();
    if (res.data) {
      chatConfig.value = {...chatConfig.value, ...res.data};
    }
  } catch (e) {
    console.error("加载对话配置失败", e);
  }
};

const handleSave = async () => {
  loading.value = true;
  try {
    await updateChatConfig(chatConfig.value);
    Message.success("保存成功");
  } catch (e) {
    Message.error("保存失败");
  } finally {
    loading.value = false;
  }
};

export default defineComponent({
  setup() {
    onMounted(() => {
      loadChatConfig();
    });

    return {
      chatConfig,
      loading,
      handleSave,
    };
  },
});
</script>

<template>
  <div class="chat-settings">
    <a-form :model="chatConfig" layout="vertical" :style="{width: '500px'}">
      <a-form-item label="最大思考步数">
        <a-input-number
          v-model="chatConfig.maxSteps"
          :min="1"
          :max="100"
          style="width: 100%"
        />
      </a-form-item>
      <a-form-item label="回答语言">
        <a-select v-model="chatConfig.language" style="width: 100%">
          <a-option value="简体中文">简体中文</a-option>
          <a-option value="English">English</a-option>
          <a-option value="繁體中文">繁體中文</a-option>
        </a-select>
      </a-form-item>
      <a-form-item label="API 重试次数">
        <a-input-number
          v-model="chatConfig.apiRetryCount"
          :min="0"
          :max="10"
          style="width: 100%"
        />
      </a-form-item>
      <a-form-item>
        <a-button type="primary" :loading="loading" @click="handleSave">保存</a-button>
      </a-form-item>
    </a-form>
  </div>
</template>
