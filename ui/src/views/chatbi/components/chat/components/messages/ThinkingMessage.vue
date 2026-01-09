<template>
  <div
    ref="thinkingContainer"
    class="message-thinking"
    v-if="message.type === 'THINKING'"
    v-html="formattedContent"
  ></div>
</template>

<script setup lang="ts">
import {computed, nextTick, ref, watch} from "vue";

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const formattedContent = computed(() => {
  return props.message.content.replace(/\n+/g, "<br>");
});

const thinkingContainer = ref<HTMLElement | null>(null);

watch(
  () => props.message.content,
  async () => {
    await nextTick();
    if (thinkingContainer.value) {
      thinkingContainer.value.scrollTop = thinkingContainer.value.scrollHeight;
    }
  },
  { immediate: true }
);
</script>

<style scoped lang="scss">
.message-thinking {
  font-size: 12px;
  color: #6a737d;
  border-left: 2px solid #dfe2e5;
  margin: 12px 0;
  padding: 8px 12px;
  background-color: #f6f8fa;
  border-radius: 0 4px 4px 0;
  max-height: 300px;
  overflow-y: auto;
}
</style>
