<template>
  <div
    v-if="message.type === 'MARKDOWN'"
    v-html="renderMarkdown(message.content)"
  ></div>
</template>

<script setup lang="ts">
import {useMarkdown} from "@/hooks/useMarkdown";

withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const { renderMarkdown } = useMarkdown();
</script>

<style scoped lang="scss"></style>
