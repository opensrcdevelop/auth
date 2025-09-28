<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <div class="chatbi-header">
      <div class="left">
        <div class="title">ChatBI 问数</div>
        <div class="info">
          基于大模型自然语言处理技术，通过语音指令实现数据查询与分析操作。
        </div>
      </div>
    </div>
    <a-tabs :active-key="activeTab" @change="handleTabChange">
      <a-tab-pane key="chat" title="对话问答">
        <div class="chat-container">
          <ChatHistory
            ref="chatHistoryRef"
            @switchChat="handleSwitchChat"
            @addNewChat="handleAddNewChat"
            @updateDataSourceId="handleUpdateDataSourceId"
          />
          <Chat
            ref="chatRef"
            :chatId="chatId"
            :dataSourceId="dataSourceId"
            @updateChatHistory="handleUpdateChatHistory"
          />
        </div>
      </a-tab-pane>
      <a-tab-pane key="data_source_management" title="数据源管理">
        <DataSourceManagement ref="dataSourceManagementRef" />
      </a-tab-pane>
      <a-tab-pane key="llm_management" title="LLM 管理"> </a-tab-pane>
    </a-tabs>
  </div>
</template>
