<template>
  <div v-if="message.type === 'DONE'">
    <div class="operator-container">
      <a-space>
        <a-tooltip content="重新生成" position="bottom" mini>
          <a-button type="text" size="small" @click="handleResendMessage">
            <template #icon>
              <icon-refresh />
            </template>
          </a-button>
        </a-tooltip>
        <a-tooltip content="复制回答" position="bottom" mini>
          <a-button
            type="text"
            size="small"
            :loading="copyDoneLoading"
            @click="handleCopyAnswer"
          >
            <template #icon>
              <icon-copy />
            </template>
          </a-button>
        </a-tooltip>
        <a-tooltip content="导出 PDF" position="bottom" mini>
          <a-button
            type="text"
            size="small"
            :loading="pdfDownloading"
            @click="downloadAsPdf"
          >
            <template #icon>
              <icon-download />
            </template>
          </a-button>
        </a-tooltip>
        <a-divider direction="vertical" />
        <a-tooltip content="喜欢" position="bottom" mini>
          <a-button
            size="mini"
            shape="circle"
            @click="handleVoteAnswer(message, 'LIKE')"
          >
            <template #icon>
              <icon-thumb-up-fill v-if="message?.feedback === 'LIKE'" />
              <icon-thumb-up v-else />
            </template>
          </a-button>
        </a-tooltip>
        <a-tooltip content="不喜欢" position="bottom" mini>
          <a-button
            size="mini"
            shape="circle"
            @click="handleVoteAnswer(message, 'DISLIKE')"
          >
            <template #icon>
              <icon-thumb-down-fill v-if="message?.feedback === 'DISLIKE'" />
              <icon-thumb-down v-else />
            </template>
          </a-button>
        </a-tooltip>
      </a-space>
      <a-space>
        <span class="model"
          >模型: {{ message.model ? message.model : "-" }}</span
        >
        <span class="token"
          >输入 token 数: {{ formatTokenCount(message.inputTokens) }}</span
        >
        <span class="token"
          >输出 token 数: {{ formatTokenCount(message.outputTokens) }}</span
        >
        <a-divider direction="vertical" />
        <span class="time">{{ message.time }}</span>
      </a-space>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref } from "vue";
import { Notification } from "@arco-design/web-vue";
import html2canvas from "html2canvas";
import jsPDF from "jspdf";
import * as echarts from "echarts";
import { voteAnswer, getCurrentUserAnswer } from "@/api/chatbi";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { useMarkdown } from "@/hooks/useMarkdown";

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  },
);

const emits = defineEmits<{
  (e: "resendMessage", questionId: string): void;
}>();

const copyDoneLoading = ref(false);
const pdfDownloading = ref(false);
const { renderMarkdown } = useMarkdown();

const formatTokenCount = (count: number): string => {
  if (!count) {
    return "-";
  }

  if (count >= 1000000) {
    return (count / 1000000).toFixed(2) + "M";
  }
  if (count >= 1000) {
    return (count / 1000).toFixed(2) + "K";
  }
  return count.toString();
};

const handleResendMessage = () => {
  emits("resendMessage", props.message.rewrittenQuestion);
};

const handleVoteAnswer = (doneMessage: any, feedback: string) => {
  const { answerId } = doneMessage;
  voteAnswer({
    answerId,
    feedback: doneMessage?.feedback === feedback ? undefined : feedback,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        doneMessage.feedback =
          doneMessage?.feedback === feedback ? undefined : feedback;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "反馈回答");
    });
};

// 复制回答内容
const handleCopyAnswer = () => {
  if (!props.message?.answerId) return;

  copyDoneLoading.value = true;
  getCurrentUserAnswer(props.message.answerId)
    .then((result: any) => {
      const answer = result?.data?.answer || "";
      navigator.clipboard.writeText(answer);
      Notification.success("复制成功");
    })
    .catch((err: any) => {
      handleApiError(err, "复制回答");
    })
    .finally(() => {
      copyDoneLoading.value = false;
    });
};

// 提取 H1 标题
const extractH1Title = (content: string): string | null => {
  const match = content.match(/^#\s+(.+)$/m);
  if (match && match[1]) {
    return match[1]
      .trim()
      .replace(/[\\/:*?"<>|]/g, "_")
      .substring(0, 50);
  }
  return null;
};

// 下载为 PDF
const downloadAsPdf = async () => {
  if (!props.message?.answerId) return;

  pdfDownloading.value = true;
  try {
    // 获取回答内容
    const result = await getCurrentUserAnswer(props.message.answerId);
    const answerContent = result?.data?.answer || "";

    // 从回答内容中提取 H1 标题
    const h1Title = extractH1Title(answerContent);

    // 创建临时容器
    const tempContainer = document.createElement("div");
    tempContainer.style.position = "absolute";
    tempContainer.style.left = "-99999px";
    tempContainer.style.top = "0";
    tempContainer.style.width = "800px";
    tempContainer.style.maxHeight = "none";
    tempContainer.style.overflow = "visible";
    tempContainer.style.background = "#ffffff";
    tempContainer.style.padding = "20px";
    tempContainer.style.boxSizing = "border-box";
    tempContainer.style.fontFamily = "Arial, sans-serif";
    tempContainer.style.fontSize = "14px";
    tempContainer.style.color = "#333333";
    tempContainer.style.lineHeight = "1.6";

    // 使用 renderMarkdown 解析 Markdown 内容
    tempContainer.innerHTML = renderMarkdown(answerContent);

    // 为 hr 分隔线添加样式
    const hrElements = tempContainer.querySelectorAll("hr");
    hrElements.forEach((el) => {
      const hrEl = el as HTMLElement;
      hrEl.style.border = "none";
      hrEl.style.height = "3px";
      hrEl.style.backgroundColor = "#dfe2e5";
      hrEl.style.margin = "22px 0";
    });

    // 创建外层容器
    const wrapper = document.createElement("div");
    wrapper.style.position = "absolute";
    wrapper.style.left = "-99999px";
    wrapper.style.top = "0";
    wrapper.appendChild(tempContainer);
    document.body.appendChild(wrapper);

    // 等待 renderMarkdown 内部的图表初始化（100ms + 自己的初始化时间）
    await new Promise((resolve) => setTimeout(resolve, 300));

    // 初始化所有图表（如果有未初始化的图表）
    const chartElements = tempContainer.querySelectorAll(".echarts-chart");
    for (const el of chartElements) {
      const chartEl = el as HTMLElement;
      const chartConfig = chartEl.dataset.chartConfig;

      if (!chartConfig) continue;

      // 如果图表已经初始化过（通过 renderMarkdown），跳过
      if (chartEl.dataset.chartInitialized) continue;

      const option = JSON.parse(decodeURIComponent(chartConfig));

      // 设置图表高度
      if (option.height) {
        chartEl.style.height = option.height;
        delete option.height;
      }

      const chart = echarts.init(chartEl);
      chart.setOption(option);
      chartEl.dataset.chartInitialized = "true";

      // 等待图表渲染完成
      await new Promise((resolve) => setTimeout(resolve, 100));

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

    document.body.removeChild(wrapper);

    const imgData = canvas.toDataURL("image/jpeg", 0.85);
    const pdfPageWidth = 595;
    const scaleToWidth = pdfPageWidth / canvas.width;
    const pdfHeight = canvas.height * scaleToWidth;

    const pdf = new jsPDF({
      orientation: "portrait",
      unit: "pt",
      format: [pdfPageWidth, pdfHeight],
    });

    pdf.addImage(imgData, "JPEG", 0, 0, pdfPageWidth, pdfHeight);
    pdf.save(h1Title ? `${h1Title}.pdf` : `answer_${Date.now()}.pdf`);
  } catch (error) {
    Notification.error("PDF 生成失败，请尝试刷新页面或稍后再试。");
    console.error("PDF 生成失败:", error);
  } finally {
    pdfDownloading.value = false;
  }
};
</script>

<style scoped lang="scss">
.operator-container {
  display: flex;
  margin-top: 8px;
  justify-content: space-between;
  align-items: center;

  .model,
  .time,
  .token {
    font-size: 12px;
    color: var(--color-text-3);
  }
}
</style>
