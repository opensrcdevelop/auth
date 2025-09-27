<template>
  <div v-if="message.type === 'HTML_REPORT'">
    <div class="html-report-container">
      <div class="content">
        <a-skeleton-line v-if="loading" :rows="6" />
        <a-skeleton-shape v-if="loading" />
        <iframe
          v-show="!loading"
          ref="htmlReportRef"
          :srcdoc="message.content"
          frameborder="0"
          width="100%"
          height="100%"
          @load="handleIframeLoad"
        />
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import {ref} from "vue";

const loading = ref(true)
const htmlReportRef = ref();

withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const emits = defineEmits<{
  (e: "ready", element: HTMLIFrameElement): void;
}>();

const handleIframeLoad = () => {
  const document = htmlReportRef.value.contentDocument || htmlReportRef.value.contentWindow?.document;
  if (document) {
    loading.value = false;
    emits("ready", htmlReportRef.value);
  }
};
</script>

<style scoped lang="scss">
.html-report-container {
  .content {
    height: 600px;
    background: none;
    border-radius: 8px;
    overflow-y: hidden;

    /* WebKit 浏览器滚动条样式 */
    &::-webkit-scrollbar {
      width: 6px;
    }

    &::-webkit-scrollbar-track {
      background: transparent;
      border-radius: 3px;
    }

    /* 滚动条滑块 */
    &::-webkit-scrollbar-thumb {
      background: rgba(144, 147, 153, 0.3);
      border-radius: 3px;

      &:hover {
        background: rgba(144, 147, 153, 0.5);
      }
    }

    /* Firefox 滚动条样式 */
    scrollbar-width: thin;
    scrollbar-color: rgba(144, 147, 153, 0.3) transparent;
  }
}
</style>
