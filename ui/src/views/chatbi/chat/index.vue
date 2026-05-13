<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <a-spin
    class="chat-box"
    :loading="globalVariables.apiLoading"
    tip="处理中，请稍候..."
  >
    <ChatHistory
      ref="chatHistoryRef"
      @switchChat="handleSwitchChat"
      @addNewChat="handleAddNewChat"
      @updateDataSourceId="handleUpdateDataSourceId"
      :dataSourceDisabled="dataSourceDisabled"
    />

    <div class="chat-main">
      <div class="chat-header">
        <div class="chat-title">{{ currentChatTitle }}</div>
        <div class="user-info">
          <div class="username">你好，</div>
          <a-link :hoverable="false" @click="handleToUserHome">{{
            currentUser.username
          }}</a-link>
          <a-button
            type="text"
            shape="circle"
            size="large"
            @click="handleLogout"
          >
            <template #icon>
              <icon-poweroff />
            </template>
          </a-button>
        </div>
      </div>

      <Chat
        ref="chatRef"
        style="width: 100%"
        :chatId="chatId"
        :dataSourceId="dataSourceId"
        :dataSourceDisabled="dataSourceDisabled"
        @updateChatHistory="handleUpdateChatHistory"
      />
    </div>
  </a-spin>
</template>
