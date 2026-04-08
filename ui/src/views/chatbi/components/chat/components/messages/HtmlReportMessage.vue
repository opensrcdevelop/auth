<template>
  <div v-if="message.type === 'HTML_REPORT'">
    <div class="html-report-container">
      <div class="operation-container">
        <a-space>
          <a-button
            type="text"
            size="mini"
            :disabled="loading"
            @click="handleOpenNewTab"
          >
            <template #icon>
              <icon-launch />
            </template>
            <template #default>新标签页打开</template>
          </a-button>
          <a-dropdown @select="handleSelectFormat">
            <a-button type="text" size="mini" :disabled="loading">
              <template #icon>
                <icon-download />
              </template>
              <template #default>下载</template>
            </a-button>
            <template #content>
              <a-doption value="pdf">PDF</a-doption>
              <a-doption value="html">HTML</a-doption>
            </template>
          </a-dropdown>
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
import html2canvas from "html2canvas";
import jsPDF from "jspdf";

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

const handleOpenNewTab = () => {
  const iframeDoc =
    htmlReportRef.value.contentDocument ||
    htmlReportRef.value.contentWindow?.document;

  if (iframeDoc) {
    const htmlContent = iframeDoc.documentElement.outerHTML;
    const blob = new Blob([htmlContent], { type: "text/html" });
    const url = URL.createObjectURL(blob);
    window.open(url, "_blank");
  }
};

const downloadAsPdf = async () => {
  const iframeDoc =
    htmlReportRef.value.contentDocument ||
    htmlReportRef.value.contentWindow?.document;

  if (!iframeDoc) return;

  try {
    const body = iframeDoc.body;

    // 确保 body 有白色背景
    body.style.backgroundColor = "#ffffff";

    const canvas = await html2canvas(body, {
      scale: 2,
      useCORS: true,
      backgroundColor: "#ffffff",
      logging: false,
      // 在克隆时确保样式正确加载
      onclone: (clonedDoc) => {
        const clonedBody = clonedDoc.body;
        clonedBody.style.backgroundColor = "#ffffff";
      },
    });

    // 生成 PDF（使用 A4 宽度，高度按比例缩放）
    const imgData = canvas.toDataURL("image/jpeg", 0.95);

    // A4 宽度（points）
    const pdfPageWidth = 595;

    // 计算缩放后的高度（保持比例，使用 A4 宽度）
    const scaleToWidth = pdfPageWidth / canvas.width;
    const pdfHeight = canvas.height * scaleToWidth;

    // 使用 A4 宽度，高度按比例
    const pdf = new jsPDF({
      orientation: "portrait",
      unit: "pt",
      format: [pdfPageWidth, pdfHeight],
    });

    pdf.addImage(imgData, "JPEG", 0, 0, pdfPageWidth, pdfHeight);

    pdf.save(`report_${new Date().getTime()}.pdf`);
  } catch (error) {
    console.error("PDF 生成失败:", error);
  }
};

const downloadAsHtml = () => {
  const iframeDoc =
    htmlReportRef.value.contentDocument ||
    htmlReportRef.value.contentWindow?.document;

  if (iframeDoc) {
    const htmlContent = iframeDoc.documentElement.outerHTML;
    const blob = new Blob([htmlContent], { type: "text/html" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = iframeDoc.title || `report_${new Date().getTime()}.html`;
    document.body.appendChild(a);
    a.click();
    setTimeout(() => {
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    }, 0);
  }
};

const handleSelectFormat = (format: string) => {
  if (format === "pdf") {
    downloadAsPdf();
  } else if (format === "html") {
    downloadAsHtml();
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
