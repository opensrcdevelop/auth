<template>
  <div
    v-if="message.type === 'MARKDOWN'"
    v-html="renderMarkdown(message.content)"
  ></div>
</template>

<script setup lang="ts">
import {md} from "@/util/markdown";
import {onMounted, onUnmounted} from "vue";

withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const renderMarkdown = (text) => {
  return md.render(text);
};

const handleCopyCode = async (event: MouseEvent) => {
  const button = event.currentTarget as HTMLButtonElement;
  const code = decodeURIComponent(button.dataset.code || "");

  try {
    await navigator.clipboard.writeText(code);
    button.textContent = "已复制";
    setTimeout(() => {
      button.textContent = "复制";
    }, 2000);
  } catch (err) {
    console.error("复制失败:", err);
  }
};

const handleFoldCode = (event: MouseEvent) => {
  const button = event.currentTarget as HTMLButtonElement;
  const container = button.closest(".code-block-container");
  const content = container?.querySelector(".code-block-content");

  if (content) {
    content.classList.toggle("folded");
    button.textContent = content.classList.contains("folded") ? "展开" : "折叠";
  }
};

onMounted(() => {
  document.addEventListener("click", (event: MouseEvent) => {
    const target = event.target as HTMLElement;

    if (target.matches(".copy-code-button")) {
      handleCopyCode(event);
    }

    if (target.matches(".fold-code-button")) {
      handleFoldCode(event);
    }
  });
});

onUnmounted(() => {
  document.removeEventListener("click", () => {});
});
</script>

<style scoped lang="scss"></style>
