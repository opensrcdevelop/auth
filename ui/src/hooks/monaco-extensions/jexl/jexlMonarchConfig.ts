import * as monaco from "monaco-editor";

const tokenizer = {
  root: [
    // 字符串：单引号或双引号
    [/"([^"\\]|\\.)*"/, "string"],
    [/'([^'\\]|\\.)*'/, "string"],
    // 数字：整数或浮点数
    [/\d+(\.\d+)?/, "number"],
    // 关键字和操作符
    [/(if|else|in|true|false|null)\b/, { cases: { $1: "keyword" } }],
    [/[|?:<>!=+\-*/&]+/, { cases: { "@operators": "operator" } }],
    // 变量：以字母或_开头，可含数字
    [
      /[a-zA-Z_]\w*/,
      {
        cases: {
          "@keywords": "keyword",
          "@default": "variable", // 非关键字视为变量
        },
      },
    ],
    // 函数调用：如 `foo()`
    [
      /([a-zA-Z_]\w*)\s*\(/,
      ["variable", { token: "delimiter.parenthesis", bracket: "@open" }],
    ],
    // 括号匹配
    [/[()]/, "@brackets"],
  ],
} as monaco.languages.IMonarchLanguage["tokenizer"];

const jexlMonarchConfig: monaco.languages.IMonarchLanguage = {
  tokenPostfix: ".jexl",
  keywords: ["if", "else", "in", "true", "false", "null"],
  operators: ["|", "+", "-", "*", "/", "&&", "||", "?:", ">", "<", "==", "!="],
  tokenizer,
};

export default jexlMonarchConfig;
