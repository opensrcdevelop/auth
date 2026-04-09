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

    <Chat
      ref="chatRef"
      style="width: 100%;"
      :chatId="chatId"
      :dataSourceId="dataSourceId"
      @updateChatHistory="handleUpdateChatHistory"
    />
  </div>
</template>
