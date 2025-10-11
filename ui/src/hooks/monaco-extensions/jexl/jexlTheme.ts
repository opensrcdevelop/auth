import * as monaco from "monaco-editor";

const jexlTheme = {
  base: "vs",
  inherit: true,
  rules: [
{ token: 'keyword.jexl', foreground: '#0000FF' }, // 蓝色关键字
    { token: 'operator.jexl', foreground: '#000000' }, // 黑色操作符
    { token: 'variable.jexl', foreground: '#0070C1' }, // 深蓝变量
    { token: 'string.jexl', foreground: '#A31515' },   // 深红字符串
    { token: 'number.jexl', foreground: '#098658' },   // 绿色数字
    // 特殊标记（如函数调用）
    { token: 'delimiter.parenthesis.jexl', foreground: '#795E26' } 
  ],
  colors: {
    'editor.foreground': '#000000', // 默认文本黑色
    'editor.background': '#FFFFFF'  // 纯白背景
  } 
} as monaco.editor.IStandaloneThemeData;

export default jexlTheme;
