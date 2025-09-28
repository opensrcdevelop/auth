<template>
  <div
    class="chat-history-container"
    :class="{
      collapsed: isCollapsed,
      overlay: isOverlay && !isCollapsed,
    }"
  >
    <div class="button-container">
      <a-button
        type="text"
        shape="circle"
        style="color: var(--color-text-3)"
        @click="toggleCollapse"
      >
        <template #icon>
          <icon-menu-fold v-if="!isCollapsed" />
          <icon-menu-unfold v-else />
        </template>
      </a-button>
      <a-button
        type="text"
        shape="circle"
        style="color: var(--color-text-3); margin-top: 4px"
        v-show="isCollapsed"
        @click="handleAddNewChat"
      >
        <template #icon>
          <icon-plus-circle />
        </template>
      </a-button>
    </div>
    <div
      class="add-chat-container"
      v-show="!isCollapsed"
      @click="handleAddNewChat"
    >
      <icon-plus-circle />
      <div class="text">开启新对话</div>
    </div>
    <div class="chat-history-list">
      <div
        class="chat-history-item"
        :class="{
          active: item.id === activeChatId,
        }"
        v-for="item in chatHistoryList"
        :key="item.id"
        v-show="!isCollapsed"
        @click="handleSwitchChat(item)"
      >
        <div class="content">
          <div
            class="title"
            :class="{
              active: item.id === activeChatId,
            }"
          >
            {{ item.title }}
          </div>
          <div class="time">{{ item.start }}</div>
        </div>
        <div class="operation">
          <a-trigger
            trigger="click"
            position="bottom"
            :popup-translate="[-50, 4]"
          >
            <a-button
              type="text"
              shape="circle"
              style="color: var(--color-text-3)"
              @click.stop="() => {}"
            >
              <template #icon>
                <icon-more />
              </template>
            </a-button>
            <template #content>
              <div class="operation-container">
                <div class="operation-item">
                  <icon-edit />
                  重命名
                </div>
                <div
                  class="operation-item"
                  style="color: #e8353e"
                  @click="handleDeleteChatHistory(item)"
                >
                  <icon-delete />
                  删除
                </div>
              </div>
            </template>
          </a-trigger>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import {deleteChatHistory, getUserChatHistory} from "@/api/chatbi";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {onMounted, onUnmounted, reactive, ref} from "vue";

const emits = defineEmits<{
  (e: "switchChat", chatId: string): void;
  (e: "addNewChat"): void;
  (e: "updateDataSourceId", dataSourceId: string);
}>();

const isCollapsed = ref(false);
const isOverlay = ref(false);
const BREAKPOINT = 768;

const handleResize = () => {
  isCollapsed.value = window.innerWidth < BREAKPOINT;
  isOverlay.value = window.innerWidth < BREAKPOINT;
};

onMounted(() => {
  handleResize();
  window.addEventListener("resize", handleResize);
});

onUnmounted(() => {
  window.removeEventListener("resize", handleResize);
});

const toggleCollapse = () => {
  isCollapsed.value = !isCollapsed.value;
  if (window.innerWidth < BREAKPOINT) {
    isOverlay.value = !isCollapsed.value;
  }
};

/**
 * 对话历史记录
 */
const chatHistoryList = reactive([]);
const handleGetChatHistoryList = () => {
  getUserChatHistory()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        chatHistoryList.length = 0;
        chatHistoryList.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取对话历史记录");
    });
};

/** 当前对话ID */
const activeChatId = ref("");

/**
 * 切换对话
 */
const handleSwitchChat = (chatHistory: any) => {
  activeChatId.value = chatHistory.id;
  emits("switchChat", chatHistory.id);
  emits("updateDataSourceId", chatHistory.dataSourceId);
};

/**
 * 开启新对话
 */
const handleAddNewChat = () => {
  activeChatId.value = "";
  emits("addNewChat");
};

/**
 * 删除对话历史记录
 */
const handleDeleteChatHistory = (chatHistory: any) => {
  Modal.warning({
    title: `确定删除对话「${chatHistory.title}（${chatHistory.start}）」吗？`,
    content: "此操作将删除该对话关联所有历史记录，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteChatHistory(chatHistory.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetChatHistoryList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除对话历史记录");
        });
    },
  });
};

const init = (chatId: string = "") => {
  activeChatId.value = chatId;
  handleGetChatHistoryList();
};

defineExpose({
  init,
});
</script>

<style scoped lang="scss">
.chat-history-container {
  background-color: #f7f8fa;
  margin-right: 16px;
  border-radius: 12px;
  transition: all 0.3s ease;
  position: relative;
  z-index: 1;
  height: calc(100vh - 200px);

  &.collapsed {
    width: 44px;
    margin-right: 8px;

    .button-container {
      align-items: center;
    }
  }

  &.overlay {
    position: absolute;
    left: 0;
    top: 0;
    height: 100%;
    z-index: 1000;
    box-shadow: 2px 0 8px rgba(0, 0, 0, 0.15);
    border-radius: 0;
  }
}

.button-container {
  display: flex;
  flex-direction: column;
  align-items: flex-end;
  margin: 8px;
}

.add-chat-container {
  display: flex;
  justify-content: center;
  align-items: center;
  padding: 8px;
  margin: 8px 8px 16px 8px;
  height: 44px;
  background-color: #fff;
  border: 1px solid var(--color-neutral-3);
  border-radius: 8px;
  cursor: pointer;
  user-select: none;
  transition: box-shadow 0.3s ease;
  transition: all 0.3s ease;

  .text {
    margin-left: 8px;
  }

  &:hover {
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
  }

  &:active {
    background-color: var(--color-neutral-2);
  }
}

.chat-history-list {
  overflow-x: hidden;
  overflow-y: auto;
  height: calc(100% - 110px);

  &::-webkit-scrollbar {
    width: 8px;
  }

  &::-webkit-scrollbar-track {
    background: transparent;
    border-radius: 3px;
  }

  &::-webkit-scrollbar-thumb {
    background: rgba(144, 147, 153, 0.3);
    border-radius: 3px;

    &:hover {
      background: rgba(144, 147, 153, 0.5);
    }
  }

  scrollbar-width: thin;
  scrollbar-color: rgba(144, 147, 153, 0.3) transparent;

  &:hover {
    scrollbar-color: rgba(144, 147, 153, 0.5) transparent;
  }
}

.chat-history-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin: 8px;
  padding: 8px;
  background-color: #fff;
  border-radius: 8px;
  width: 240px;
  cursor: pointer;
  transition: all 0.3s ease;

  &:hover {
    background-color: var(--color-neutral-2);
  }

  &.active {
    background-color: #e4edfd;
  }

  .content {
    .title {
      font-size: 14px;
      color: var(--color-neutral-10);
      margin-bottom: 4px;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;

      &.active {
        color: #3964fe;
      }
    }

    .time {
      font-size: 12px;
      color: var(--color-neutral-6);
    }
  }
}

.operation-container {
  background-color: #fff;
  box-shadow: 0 0 12px 0 #eceef4;
  border-radius: 8px;
  padding: 8px;
  box-sizing: border-box;

  .operation-item {
    width: 120px;
    border-radius: 4px;
    padding: 8px 16px;
    color: #293350;
    cursor: pointer;
    line-height: 22px;
    font-weight: 400;
  }

  .operation-item:hover {
    background-color: #eff2f6;
  }
}
</style>
