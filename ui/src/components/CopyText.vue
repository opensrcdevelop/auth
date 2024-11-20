<template>
  <a-tooltip :content="tipText" position="top">
    <div class="text-container" @click="copyText">
      <span :style="{ color: props.textColor }">{{ props.text }}</span>
      <icon-copy class="icon" />
    </div>
  </a-tooltip>
</template>

<script setup lang="ts">
import { ref } from "vue";

const props = defineProps({
  text: {
    type: String,
    default: "",
  },
  textColor: {
    type: String,
    default: "#1d2129",
    required: false,
  },
});

const tipText = ref("点击复制");

const copyText = () => {
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(props.text);
    tipText.value = "已复制到剪切板";
  } else {
    // 创建 text area
    const textArea = document.createElement("textarea");
    textArea.value = props.text;
    // 使 text area 不在 viewport，同时设置不可见
    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();
    // 执行复制命令并移除文本框
    document.execCommand("copy");
    textArea.remove();
    tipText.value = "已复制到剪切板";
  }
  setTimeout(() => {
    tipText.value = "点击复制";
  }, 1000);
};
</script>

<style scoped lang="scss">
.text-container {
  display: flex;
  align-items: center;
  cursor: pointer;

  .icon {
    margin-left: 4px;
    color: #86909c;
  }
}
</style>
