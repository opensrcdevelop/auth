<template>
  <div v-if="message.type === 'THINKING'" class="message-thinking-wrapper">
    <div class="thinking-header" @click="toggleCollapse">
      <icon-caret-down
        class="collapse-icon"
        :class="{ collapsed: isCollapsed }"
      />
      <span class="thinking-label">思考过程</span>
    </div>
    <div
      ref="thinkingContainer"
      class="message-thinking"
      v-show="!isCollapsed"
      v-html="renderMarkdown(message.content)"
    ></div>
  </div>
</template>

<script setup lang="ts">
import { useMarkdown } from "@/hooks/useMarkdown";
import { computed, nextTick, ref, watch } from "vue";

const { renderMarkdown } = useMarkdown();

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  },
);

const formattedContent = computed(() => {
  return props.message.content.replace(/\n+/g, "<br>");
});

const thinkingContainer = ref<HTMLElement | null>(null);
const isCollapsed = ref(false);

const toggleCollapse = () => {
  isCollapsed.value = !isCollapsed.value;
};

watch(
  () => props.message.content,
  async () => {
    await nextTick();
    if (thinkingContainer.value && !isCollapsed.value) {
      thinkingContainer.value.scrollTop = thinkingContainer.value.scrollHeight;
    }
  },
  { immediate: true },
);

// 监听 DONE 消息出现，自动折叠
watch(
  () => props.message.done,
  (done) => {
    if (done) {
      isCollapsed.value = true;
    }
  },
  { immediate: true },
);
</script>

<style scoped lang="scss">
.message-thinking-wrapper {
  font-size: 12px;
  color: #6a737d;
  border-left: 2px solid #dfe2e5;
  margin: 12px 0;
  padding-left: 12px;
  background-color: #fff;
  border-radius: 0 4px 4px 0;
}

.thinking-header {
  display: flex;
  align-items: center;
  gap: 4px;
  padding: 4px 0;
  cursor: pointer;
  user-select: none;

  &:hover {
    .thinking-label {
      color: #4b5a6a;
    }
  }

  .collapse-icon {
    transition: transform 0.2s;
    transform: rotate(0deg);

    &.collapsed {
      transform: rotate(-90deg);
    }
  }

  .thinking-label {
    font-weight: 500;
    color: #6a737d;
  }
}

.message-thinking {
  max-height: 300px;
  overflow-y: auto;
  padding: 4px;
  margin-top: 4px;
}
</style>
