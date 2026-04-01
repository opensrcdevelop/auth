<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div class="chat-page" :class="{ 'embed-mode': embedMode }">
    <div class="chat-header" v-if="!embedMode">
      <div class="left">
        <div class="title">ChatBI 问数</div>
        <div class="info">
          基于大模型自然语言处理技术，通过语音指令实现数据查询与分析操作。
        </div>
      </div>
      <div class="right">
        <a-button type="text" @click="showSettings = true">
          <template #icon><icon-settings /></template>
          对话设置
        </a-button>
      </div>
    </div>
    <div class="chat-container" :class="{ 'full-screen': embedMode }">
      <ChatHistory
        ref="chatHistoryRef"
        @switchChat="handleSwitchChat"
        @addNewChat="handleAddNewChat"
        @updateDataSourceId="handleUpdateDataSourceId"
        v-if="!embedMode"
      />
      <Chat
        ref="chatRef"
        :chatId="chatId"
        :dataSourceId="dataSourceId"
        @updateChatHistory="handleUpdateChatHistory"
      />
    </div>
    <!-- 设置弹窗 -->
    <a-modal
      v-model:visible="showSettings"
      title="对话设置"
      :footer="false"
      width="500px"
    >
      <a-form :model="chatConfig" layout="vertical">
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
        <a-form-item label="API 超时时间（秒）">
          <a-input-number
            v-model="chatConfig.apiTimeout"
            :min="30"
            :max="600"
            style="width: 100%"
          />
        </a-form-item>
        <a-form-item>
          <a-space>
            <a-button type="primary" @click="handleSaveConfig">保存</a-button>
            <a-button @click="showSettings = false">取消</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </a-modal>
  </div>
</template>
