<template>
  <div v-if="message.type === 'HTML_REPORT'">
    <div class="html-report-container">
      <div class="content">
        <iframe
          ref="htmlReportRef"
          :srcdoc="message.content"
          frameborder="0"
          width="100%"
          height="100%"
        />
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import html2canvas from "html2canvas";
import jsPDF from "jspdf";
import {ref} from "vue";

const htmlReportRef = ref();

withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const toFullscreen = () => {
  if (htmlReportRef.value?.requestFullscreen) {
    htmlReportRef.value.requestFullscreen();
  }
};

const downloadAsPdf = async () => {
  try {
    const document =
      htmlReportRef.value.contentDocument ||
      htmlReportRef.value.contentWindow?.document;
    if (!document) {
      console.error("无法获取 iframe 的 document 对象");
      return;
    }
    const title = document.title || `report_${new Date().getTime()}`;
    const canvas = await html2canvas(document.body, {
      scale: 2,
      useCORS: true,
      allowTaint: true,
      logging: false,
      width: document.body.scrollWidth,
      height: document.body.scrollHeight,
    });

    const imgData = canvas.toDataURL("image/jpeg", 0.3);
    const pdf = new jsPDF({
      orientation: canvas.width > canvas.height ? "landscape" : "portrait",
      unit: "px",
      format: [canvas.width, canvas.height],
      compress: true,
    });
    pdf.addImage(imgData, "JPEG", 15, 40, 180, 160, undefined, "FAST");
    pdf.save(`${title}.pdf`);
  } catch (error) {
    console.error("下载PDF失败:", error);
  }
};

defineExpose({
  toFullscreen,
  downloadAsPdf,
});
</script>

<style scoped lang="scss"></style>
