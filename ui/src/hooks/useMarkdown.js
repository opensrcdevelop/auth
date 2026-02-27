var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
import hljs from "highlight.js";
import MarkdownIt from "markdown-it";
import { onMounted, onUnmounted } from "vue";
import MarkdownHandler from "./md/MarkdownHandler";
import { copyToClipboard } from "@/util/tool";
var md = new MarkdownIt({
    html: true,
    linkify: true,
    typographer: true,
    breaks: true,
    highlight: function (str, lang) {
        if (lang && hljs.getLanguage(lang)) {
            try {
                var highlightedCode = hljs.highlight(str, { language: lang }).value;
                return [
                    "<div class=\"code-block-container\">",
                    "<div class=\"code-block-header\">",
                    "<span class=\"code-language\">".concat(lang, "</span>"),
                    "<div class=\"code-block-buttons\">",
                    "<button class=\"copy-code-button\" data-code=\"".concat(encodeURIComponent(str), "\">\u590D\u5236</button>"),
                    "<button class=\"fold-code-button\">\u5C55\u5F00</button>",
                    "</div>",
                    "</div>",
                    "<div class=\"code-block-content folded\">",
                    "<pre class=\"hljs\"><code>".concat(highlightedCode, "</code></pre>"),
                    "</div>",
                    "</div>",
                ].join("");
            }
            catch (_) { }
        }
        return [
            "<div class=\"code-block-container\">",
            "<div class=\"code-block-header\">",
            "<span class=\"code-language\">text</span>",
            "<button class=\"copy-code-button\" data-code=\"".concat(encodeURIComponent(str), "\">\u590D\u5236</button>"),
            "</div>",
            "<pre class=\"hljs\"><code>".concat(md.utils.escapeHtml(str), "</code></pre>"),
            "</div>",
        ].join("");
    },
});
md.renderer.rules.code_inline = function (tokens, idx) {
    var token = tokens[idx];
    return [
        "<div class=\"inline-code-container\">",
        "<code class=\"inline-code\">".concat(md.utils.escapeHtml(token.content), "</code>"),
        "</div>",
    ].join("");
};
export function useMarkdown() {
    var _this = this;
    var handlerState = MarkdownHandler.getInstance();
    var handleCopyCode = function (button) { return __awaiter(_this, void 0, void 0, function () {
        var code, originalText, success, err_1;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    code = decodeURIComponent(button.dataset.code || "");
                    originalText = button.textContent;
                    if (button.disabled)
                        return [2 /*return*/];
                    _a.label = 1;
                case 1:
                    _a.trys.push([1, 3, , 4]);
                    return [4 /*yield*/, copyToClipboard(code)];
                case 2:
                    success = _a.sent();
                    if (success) {
                        button.textContent = "已复制";
                        button.disabled = true;
                        setTimeout(function () {
                            button.textContent = originalText;
                            button.disabled = false;
                        }, 2000);
                    }
                    return [3 /*break*/, 4];
                case 3:
                    err_1 = _a.sent();
                    console.error("复制失败:", err_1);
                    return [3 /*break*/, 4];
                case 4: return [2 /*return*/];
            }
        });
    }); };
    var handleToggleFoldCode = function (button) {
        var container = button.closest(".code-block-container");
        var content = container === null || container === void 0 ? void 0 : container.querySelector(".code-block-content");
        if (content) {
            content.classList.toggle("folded");
            button.textContent = content.classList.contains("folded")
                ? "展开"
                : "折叠";
        }
    };
    onMounted(function () {
        if (handlerState.isBound && handlerState.handler) {
            document.removeEventListener("click", handlerState.handler, true);
            handlerState.handler = null;
            handlerState.isBound = false;
        }
        ;
        handlerState.handler = function (event) {
            var _a, _b;
            var target = event.target;
            if (target.nodeType !== Node.ELEMENT_NODE) {
                return;
            }
            var copyButton = (_a = target.closest) === null || _a === void 0 ? void 0 : _a.call(target, ".copy-code-button");
            var foldButton = (_b = target.closest) === null || _b === void 0 ? void 0 : _b.call(target, ".fold-code-button");
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
    onUnmounted(function () {
        if (handlerState.handler) {
            document.removeEventListener("click", handlerState.handler, true);
            handlerState.handler = null;
            handlerState.isBound = false;
        }
    });
    var renderMarkdown = function (content) {
        var rendered = md.render(content, {
            breaks: true,
            gfm: true
        });
        return "<div class=\"markdown-body\">".concat(rendered, "</div>");
    };
    return {
        md: md,
        renderMarkdown: renderMarkdown,
    };
}
