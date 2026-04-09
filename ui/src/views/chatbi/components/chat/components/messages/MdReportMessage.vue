<template>
  <div v-if="message.type === 'MD_REPORT'">
    <div class="md-report-container">
      <div class="operation-container">
        <a-space>
          <a-dropdown @select="handleSelectFormat">
            <a-button type="text" size="mini">
              <template #icon>
                <icon-download />
              </template>
              <template #default>下载</template>
            </a-button>
            <template #content>
              <a-doption value="pdf" :disabled="pdfDownloading">PDF</a-doption>
              <a-doption value="md">Markdown</a-doption>
            </template>
          </a-dropdown>
        </a-space>
      </div>
      <div ref="reportContainerRef" class="content markdown-body">
        <div v-html="renderMarkdown(message.content)" />
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import {ref} from "vue";
import {useMarkdown} from "@/hooks/useMarkdown";
import html2canvas from "html2canvas";
import jsPDF from "jspdf";
import * as echarts from "echarts";
import {Notification} from "@arco-design/web-vue";

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const reportContainerRef = ref<HTMLElement | null>(null);
const {renderMarkdown} = useMarkdown();

const pdfDownloading = ref(false);
const downloadAsPdf = async () => {
  const container = reportContainerRef.value;
  if (!container) return;

  const content = props.message?.content || "";
  const h1Title = extractH1Title(content);

  pdfDownloading.value = true;
  try {
    // 创建临时容器用于 PDF 生成
    const tempContainer = container.cloneNode(true) as HTMLElement;
    tempContainer.style.position = "absolute";
    tempContainer.style.left = "-99999px";
    tempContainer.style.top = "0";
    tempContainer.style.width = container.scrollWidth + "px";
    tempContainer.style.maxHeight = "none";
    tempContainer.style.overflow = "visible";
    tempContainer.style.background = "#ffffff";
    tempContainer.style.padding = "20px";
    tempContainer.style.boxSizing = "border-box";

    // 为 hr 分隔线添加样式
    const hrElements = tempContainer.querySelectorAll("hr");
    hrElements.forEach((el) => {
      const hrEl = el as HTMLElement;
      hrEl.style.border = "none";
      hrEl.style.height = "3px";
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

    // 初始化所有图表
    const chartElements = tempContainer.querySelectorAll(".echarts-chart");

    for (const el of chartElements) {
      const chartEl = el as HTMLElement;
      const chartConfig = chartEl.dataset.chartConfig;

      if (!chartConfig) continue;

      const option = JSON.parse(decodeURIComponent(chartConfig));
      let chart = echarts.getInstanceByDom(chartEl);
      if (!chart) {
        chart = echarts.init(chartEl);
      }
      chart.setOption(option);

      // getDataURL 会自动等待渲染完成
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
    }

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

    pdf.save(h1Title ? `${h1Title}.pdf` : `report_${Date.now()}.pdf`);
  } catch (error) {
    Notification.error("PDF 生成失败，请尝试刷新页面或稍后再试。")
    console.error("PDF 生成失败:", error);
  } finally {
    pdfDownloading.value = false;
  }
};

const extractH1Title = (content: string): string | null => {
  // 匹配以 # 开头的 H1 标题
  const match = content.match(/^#\s+(.+)$/m);
  if (match && match[1]) {
    // 清理标题：移除特殊字符，只保留有效文件名字符
    return match[1]
      .trim()
      .replace(/[\\/:*?"<>|]/g, "_")
      .substring(0, 50);
  }
  return null;
};

const downloadAsMarkdown = () => {
  const content = props.message?.content || "";
  const blob = new Blob([content], {type: "text/markdown;charset=utf-8"});
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  const h1Title = extractH1Title(content);
  a.download = h1Title ? `${h1Title}.md` : `report_${Date.now()}.md`;
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
      background-color: #fff;
      color: #24292e;
    }
  }
}
</style>
