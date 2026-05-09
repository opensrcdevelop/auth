import hljs from "highlight.js";
import MarkdownIt from "markdown-it";
import {nextTick, onMounted, onUnmounted} from "vue";
import MarkdownHandler from "./md/MarkdownHandler";
import {copyToClipboard} from "@/util/tool";
import * as echarts from "echarts";

const md: any = new MarkdownIt({
  html: true,
  linkify: true,
  typographer: true,
  breaks: true,
  highlight: (str, lang) => {
    if (lang && hljs.getLanguage(lang)) {
      try {
        const highlightedCode = hljs.highlight(str, { language: lang }).value;
        return [
          `<div class="code-block-container">`,
          `<div class="code-block-header">`,
          `<span class="code-language">${lang}</span>`,
          `<div class="code-block-buttons">`,
          `<button class="copy-code-button" data-code="${encodeURIComponent(
            str
          )}">复制</button>`,
          `<button class="fold-code-button">展开</button>`,
          `</div>`,
          `</div>`,
          `<div class="code-block-content folded">`,
          `<pre class="hljs"><code>${highlightedCode}</code></pre>`,
          `</div>`,
          `</div>`,
        ].join("");
      } catch (_) {}
    }
    return [
      `<div class="code-block-container">`,
      `<div class="code-block-header">`,
      `<span class="code-language">text</span>`,
      `<button class="copy-code-button" data-code="${encodeURIComponent(
        str
      )}">复制</button>`,
      `</div>`,
      `<pre class="hljs"><code>${md.utils.escapeHtml(str)}</code></pre>`,
      `</div>`,
    ].join("");
  },
});

// 自定义 hr 渲染规则
md.renderer.rules.hr = (tokens: any, idx: any) => {
  return '<hr class="md-hr">';
};

md.renderer.rules.code_inline = (tokens: any, idx: any) => {
  const token = tokens[idx];
  return [
    `<div class="inline-code-container">`,
    `<code class="inline-code">${md.utils.escapeHtml(token.content)}</code>`,
    `</div>`,
  ].join("");
};

// 自定义表格渲染规则 - 包装表格容器支持滚动
md.renderer.rules.table_open = () => {
  return '<div class="table-container"><table>';
};

md.renderer.rules.table_close = () => {
  return '</table></div>';
};

// 递归转换 JSON 中的函数字符串为真正的函数
function parseFunctionStrings(obj: any): any {
  if (obj === null || obj === undefined) return obj;
  if (typeof obj === "string") {
    // 匹配 function 开头的函数字符串
    if (obj.trim().startsWith("function")) {
      try {
        // eslint-disable-next-line no-eval
        return eval("(" + obj + ")");
      } catch (e) {
        console.error("函数转换失败:", e);
        return obj;
      }
    }
    return obj;
  }
  if (Array.isArray(obj)) {
    return obj.map((item) => parseFunctionStrings(item));
  }
  if (typeof obj === "object") {
    const result: any = {};
    for (const key in obj) {
      if (obj.hasOwnProperty(key)) {
        result[key] = parseFunctionStrings(obj[key]);
      }
    }
    return result;
  }
  return obj;
}

export function useMarkdown() {
  const handlerState = MarkdownHandler.getInstance();

  const handleCopyCode = async (button: HTMLButtonElement) => {
    const code = decodeURIComponent(button.dataset.code || "");
    const originalText = button.textContent;

    if (button.disabled) return;

    try {
      const success = await copyToClipboard(code);

      if (success) {
        button.textContent = "已复制";
        button.disabled = true;

        setTimeout(() => {
          button.textContent = originalText;
          button.disabled = false;
        }, 2000);
      }
    } catch (err) {
      console.error("复制失败:", err);
    }
  };

  const handleToggleFoldCode = (button: HTMLButtonElement) => {
    const container = button.closest(".code-block-container");
    const content = container?.querySelector(".code-block-content");

    if (content) {
      content.classList.toggle("folded");
      button.textContent = content.classList.contains("folded")
        ? "展开"
        : "折叠";
    }
  };

  onMounted(() => {
    if (handlerState.isBound && handlerState.handler) {
      document.removeEventListener("click", handlerState.handler, true);
      handlerState.handler = null;
      handlerState.isBound = false;
    };

    handlerState.handler = (event: MouseEvent) => {
      let target = event.target as HTMLElement;

      if (target.nodeType !== Node.ELEMENT_NODE) {
        return;
      }

      const copyButton = target.closest?.(
        ".copy-code-button"
      ) as HTMLButtonElement;
      const foldButton = target.closest?.(
        ".fold-code-button"
      ) as HTMLButtonElement;

      if (copyButton) {
        event.preventDefault();
        event.stopPropagation();
        handleCopyCode(copyButton);
        return;
      }

      if (foldButton) {
        event.preventDefault();
        event.stopPropagation();
        handleToggleFoldCode(foldButton);
        return;
      }
    };

    document.addEventListener("click", handlerState.handler, true);
    handlerState.isBound = true;
  });

  onUnmounted(() => {
    if (handlerState.handler) {
      document.removeEventListener("click", handlerState.handler, true);
      handlerState.handler = null;
      handlerState.isBound = false;
    }
  });

  const renderMarkdown = (content: string): string => {
    // 提取并处理 echarts 代码块
    const processedContent = content.replace(/```echarts\n([\s\S]*?)```/g, (match, jsonStr) => {
      try {
        const chartId = `echarts_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
        return `<div class="echarts-chart" data-chart-id="${chartId}" data-chart-config="${encodeURIComponent(jsonStr.trim())}"></div>`;
      } catch (e) {
        console.error("解析 echarts 配置失败:", e);
        return match;
      }
    });

    const rendered = md.render(processedContent, {
      breaks: true,
      gfm: true
    });

    // 延迟初始化 echarts 图表
    nextTick(() => {
      setTimeout(() => {
        document.querySelectorAll(".echarts-chart").forEach((el) => {
          const chartEl = el as HTMLElement;
          const chartConfig = chartEl.dataset.chartConfig;
          if (chartConfig && !chartEl.dataset.chartInitialized) {
            try {
              let option = JSON.parse(decodeURIComponent(chartConfig));
              // 转换函数字符串为真正的函数
              option = parseFunctionStrings(option);
              // 设置图表高度
              if (option.height) {
                chartEl.style.height = option.height;
                delete option.height;
              }
              const chart = echarts.init(chartEl);
              chart.setOption(option);
              chartEl.dataset.chartInitialized = "true";

              // 响应式调整
              const resizeObserver = new ResizeObserver(() => {
                chart.resize();
              });
              resizeObserver.observe(chartEl);
            } catch (e) {
              console.error("初始化 echarts 图表失败:", e);
              const errorMsg = e instanceof Error ? e.message : "未知错误";
              // 设置默认高度以确保能垂直居中
              if (!chartEl.style.height || chartEl.style.height === "0px" || chartEl.style.height === "") {
                chartEl.style.height = "300px";
              }
              chartEl.innerHTML = `
                <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; width: 100%; height: 100%; color: #999; font-size: 14px; padding: 16px; text-align: center; box-sizing: border-box;">
                  <div style="margin-bottom: 8px; font-weight: 500;">图表加载失败</div>
                  <div style="font-size: 12px; color: #666; word-break: break-word; overflow-wrap: break-word; white-space: pre-wrap;">${errorMsg}</div>
                </div>`;
            }
          }
        });
      }, 100);
    });

    return `<div class="markdown-body">${rendered}</div>`;
  };

  return {
    md,
    renderMarkdown,
  };
}
