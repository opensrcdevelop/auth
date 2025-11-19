<template>
  <div v-if="message.type === 'HTML_REPORT'">
    <div class="html-report-container">
      <div class="operation-container">
        <a-space>
          <a-button
            type="text"
            size="mini"
            :disabled="loading"
            @click="handleFullscreen"
          >
            <template #icon>
              <icon-fullscreen />
            </template>
            <template #default>全屏显示</template>
          </a-button>
          <a-button
            type="text"
            size="mini"
            :disabled="loading"
            @click="handleDownloadReport"
          >
            <template #icon>
              <icon-download />
            </template>
            <template #default>下载</template>
          </a-button>
        </a-space>
      </div>
      <div class="content">
        <a-skeleton v-if="loading" animation >
          <a-skeleton-line :rows="16" />
        </a-skeleton>
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

const loading = ref(true);
const htmlReportRef = ref();

withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const handleIframeLoad = () => {
  const document =
    htmlReportRef.value.contentDocument ||
    htmlReportRef.value.contentWindow?.document;
  if (document) {
    loading.value = false;
  }
};

const handleFullscreen = () => {
  htmlReportRef.value.requestFullscreen();
};

const handleDownloadReport = () => {
  const iframeDoc =
    htmlReportRef.value.contentDocument ||
    htmlReportRef.value.contentWindow?.document;

  if (iframeDoc) {
    const htmlContent = iframeDoc.documentElement.outerHTML;
    const blob = new Blob([htmlContent], { type: "text/html" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = iframeDoc.title || `report_${new Date().getTime()}`;
    document.body.appendChild(a);
    a.click();
    setTimeout(() => {
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    }, 0);
  }
};
</script>

<style scoped lang="scss">
.html-report-container {
  .operation-container {
    display: flex;
    justify-content: flex-end;
    align-items: center;
    margin-bottom: 6px;
    padding: 2px;
    background-color: #f6f8fa;
    border-radius: 4px;
  }
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
