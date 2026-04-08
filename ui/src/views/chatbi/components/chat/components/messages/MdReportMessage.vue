<template>
  <div v-if="message.type === 'MD_REPORT'">
    <div class="md-report-container">
      <div class="operation-container">
        <a-space>
          <a-dropdown @select="handleSelectFormat">
            <a-button type="text" size="mini" :disabled="loading">
              <template #icon>
                <icon-download />
              </template>
              <template #default>下载</template>
            </a-button>
            <template #content>
              <a-doption value="pdf">PDF</a-doption>
              <a-doption value="md">Markdown</a-doption>
            </template>
          </a-dropdown>
        </a-space>
      </div>
      <div ref="reportContainerRef" class="content markdown-body">
        <a-skeleton v-if="loading" animation>
          <a-skeleton-line :rows="16" />
        </a-skeleton>
        <div v-show="!loading" v-html="renderMarkdown(message.content)"></div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import {nextTick, ref} from "vue";
import {useMarkdown} from "@/hooks/useMarkdown";
import html2canvas from "html2canvas";
import jsPDF from "jspdf";
import * as echarts from "echarts";

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const loading = ref(true);
const reportContainerRef = ref<HTMLElement | null>(null);
const {renderMarkdown} = useMarkdown();

nextTick(() => {
  loading.value = false;
});

const downloadAsPdf = async () => {
  const container = reportContainerRef.value;
  if (!container) return;

  try {
    // 创建临时容器用于 PDF 生成（完全隐藏）
    const tempContainer = container.cloneNode(true) as HTMLElement;
    tempContainer.style.position = "absolute";
    tempContainer.style.left = "-99999px";
    tempContainer.style.top = "0";
    tempContainer.style.width = container.scrollWidth + "px";
    tempContainer.style.maxHeight = "none";
    tempContainer.style.overflow = "visible";
    tempContainer.style.background = "#ffffff";
    tempContainer.style.visibility = "hidden";
    tempContainer.style.padding = "20px";
    tempContainer.style.boxSizing = "border-box";

    // 移除不需要的元素
    const skeleton = tempContainer.querySelector(".arco-skeleton");
    if (skeleton) skeleton.remove();

    // 为图表容器添加样式
    const chartContainers = tempContainer.querySelectorAll(".echarts-chart");
    chartContainers.forEach((el) => {
      const chartEl = el as HTMLElement;
      chartEl.style.minHeight = "300px";
      chartEl.style.margin = "20px 0";
      chartEl.style.borderRadius = "8px";
      chartEl.style.border = "1px solid #dfe2e5";
    });

    // 为 hr 分隔线添加样式
    const hrElements = tempContainer.querySelectorAll("hr");
    hrElements.forEach((el) => {
      const hrEl = el as HTMLElement;
      hrEl.style.border = "none";
      hrEl.style.height = "2px";
      hrEl.style.backgroundColor = "#dfe2e5";
      hrEl.style.margin = "22px 0";
    });

    // 创建外层容器用于显示
    const wrapper = document.createElement("div");
    wrapper.style.position = "absolute";
    wrapper.style.left = "-99999px";
    wrapper.style.top = "0";
    wrapper.appendChild(tempContainer);
    document.body.appendChild(wrapper);

    // 等待 DOM 更新
    await new Promise((resolve) => setTimeout(resolve, 100));

    // 强制显示以便截图
    tempContainer.style.visibility = "visible";

    // 初始化所有图表
    const chartElements = tempContainer.querySelectorAll(".echarts-chart");

    for (const el of chartElements) {
      const chartEl = el as HTMLElement;
      const chartConfig = chartEl.dataset.chartConfig;

      if (!chartConfig) continue;

      const option = JSON.parse(decodeURIComponent(chartConfig));
      const chart = echarts.init(chartEl);
      chart.setOption(option);

      // 等待渲染
      await new Promise((resolve) => setTimeout(resolve, 800));

      // 转换为图片
      const imgUrl = chart.getDataURL({
        type: "png",
        pixelRatio: 2,
        backgroundColor: "#ffffff",
      });

      const img = document.createElement("img");
      img.src = imgUrl;
      img.style.width = "100%";
      img.style.height = "100%";
      img.style.display = "block";

      chartEl.innerHTML = "";
      chartEl.appendChild(img);
      chart.dispose();
    }

    // 等待图片渲染
    await new Promise((resolve) => setTimeout(resolve, 300));

    const scale = 2;

    // 截图生成 PDF
    const canvas = await html2canvas(tempContainer, {
      scale,
      useCORS: true,
      backgroundColor: "#ffffff",
      logging: false,
    });

    // 清理临时容器
    document.body.removeChild(wrapper);

    // 生成 PDF（使用 A4 宽度，高度按比例缩放）
    const imgData = canvas.toDataURL("image/jpeg", 0.85);

    // A4 宽度（points）
    const pdfPageWidth = 595;
    const pdfPageHeight = 842;

    // 计算缩放后的高度（保持比例，使用 A4 宽度）
    const scaleToWidth = pdfPageWidth / canvas.width;
    const pdfHeight = canvas.height * scaleToWidth;

    // 根据内容选择方向
    const pdf = new jsPDF({
      orientation: "portrait",
      unit: "pt",
      format: [pdfPageWidth, pdfHeight],
    });

    pdf.addImage(imgData, "JPEG", 0, 0, pdfPageWidth, pdfHeight);

    pdf.save(`report_${Date.now()}.pdf`);
  } catch (error) {
    console.error("PDF 生成失败:", error);
  }
};

const downloadAsMarkdown = () => {
  const content = props.message?.content || "";
  const blob = new Blob([content], {type: "text/markdown;charset=utf-8"});
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = `report_${Date.now()}.md`;
  document.body.appendChild(a);
  a.click();
  setTimeout(() => {
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }, 0);
};

const handleSelectFormat = (format: string) => {
  if (format === "pdf") {
    downloadAsPdf();
  } else if (format === "md") {
    downloadAsMarkdown();
  }
};
</script>

<style scoped lang="scss">
.md-report-container {
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
    background: #ffffff;
    border-radius: 8px;
    padding: 16px;
    max-height: 600px;
    overflow-y: auto;

    :deep(.markdown-body) {
      background: #ffffff;
      color: #24292e;
    }

    :deep(.echarts-chart) {
      width: 100%;
      min-height: 300px;
      max-height: 500px;
      margin: 16px 0;
      border-radius: 8px;
      background: #fff;
      border: 1px solid #dfe2e5;
    }

    :deep(.md-hr) {
      border: none;
      height: 2px;
      background: #dfe2e5;
      margin: 22px 0;
    }

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
