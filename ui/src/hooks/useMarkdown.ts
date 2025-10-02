import hljs from "highlight.js";
import MarkdownIt from "markdown-it";
import {onMounted, onUnmounted} from "vue";
import MarkdownHandler from "./md/MarkdownHandler";

const md = new MarkdownIt({
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

md.renderer.rules.code_inline = (tokens, idx) => {
  const token = tokens[idx];
  return [
    `<div class="inline-code-container">`,
    `<code class="inline-code">${md.utils.escapeHtml(token.content)}</code>`,
    `</div>`,
  ].join("");
};

export function useMarkdown() {
  const handlerState = MarkdownHandler.getInstance();

  const copyToClipboard = async (text: string): Promise<boolean> => {
    if (navigator.clipboard && window.isSecureContext) {
      try {
        await navigator.clipboard.writeText(text);
        return true;
      } catch (err) {
        console.warn("Clipboard API 失败:", err);
      }
    }

    try {
      const textArea = document.createElement("textarea");
      textArea.value = text;

      textArea.style.position = "fixed";
      textArea.style.top = "0";
      textArea.style.left = "0";
      textArea.style.width = "2em";
      textArea.style.height = "2em";
      textArea.style.padding = "0";
      textArea.style.border = "none";
      textArea.style.outline = "none";
      textArea.style.boxShadow = "none";
      textArea.style.background = "transparent";
      textArea.style.opacity = "0";

      document.body.appendChild(textArea);
      textArea.select();
      textArea.setSelectionRange(0, 99999);

      const successful = document.execCommand("copy");
      document.body.removeChild(textArea);

      if (successful) {
        return true;
      }
    } catch (err) {
      console.warn("execCommand 方法失败:", err);
    }

    try {
      const selection = document.getSelection();
      const range = document.createRange();
      const div = document.createElement("div");
      div.textContent = text;

      document.body.appendChild(div);
      range.selectNodeContents(div);
      selection?.removeAllRanges();
      selection?.addRange(range);

      const successful = document.execCommand("copy");
      selection?.removeAllRanges();
      document.body.removeChild(div);

      if (successful) {
        return true;
      }
    } catch (err) {
      console.warn("Selection API 方法失败:", err);
    }
  };

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
    const rendered = md.render(content);
    return `<div class="markdown-body">${rendered}</div>`;
  };

  return {
    md,
    renderMarkdown,
  };
}
