<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div class="chat-box">
    <ChatHistory
      ref="chatHistoryRef"
      @switchChat="handleSwitchChat"
      @addNewChat="handleAddNewChat"
      @updateDataSourceId="handleUpdateDataSourceId"
    />

    <div class="chat-main">
      <div class="chat-header">
        <div class="chat-title">{{ currentChatTitle }}</div>
        <div class="user-info">
          <div class="username">你好，</div>
          <a-link :hoverable="false" @click="handleToUserHome">{{ currentUser.username }}</a-link>
          <a-button type="text" shape="circle" size="large" @click="handleLogout">
            <template #icon>
              <icon-poweroff />
            </template>
          </a-button>
        </div>
      </div>

      <Chat
        ref="chatRef"
        style="width: 100%;"
        :chatId="chatId"
        :dataSourceId="dataSourceId"
        @updateChatHistory="handleUpdateChatHistory"
      />
    </div>
  </div>
</template>
