<template>
  <div
    class="message-thinking"
    v-if="message.type === 'THINKING'"
    v-html="formattedContent"
  ></div>
</template>

<script setup lang="ts">
import {computed} from "vue";

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const formattedContent = computed(() => {
  return props.message.content
    .replace(/\n+/g, "<br>");
});
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
}
</style>
