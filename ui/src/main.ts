import "@/style/index.scss";

import { createApp } from "vue";

import App from "./App.vue";
import router from "./router";
import pina from "./store";

import ArcoVue, { Icon } from "@arco-design/web-vue";
import ArcoVueIcon from "@arco-design/web-vue/es/icon";
import "@arco-design/web-vue/dist/arco.css";
import "ant-design-vue/dist/reset.css";
import { QRCode } from "ant-design-vue";
import PageHeader from "@/components/PageHeader.vue";
import CopyText from "@/components/CopyText.vue";
import InputText from "@/components/InputText.vue";
import Authorize from "@/components/Authorize.vue";
import Verify from "@/components/verifition/Verify.vue";
import MonacoEditor from "./components/MonacoEditor.vue";

const app = createApp(App);

app.use(pina);
app.use(router);
app.use(ArcoVue);
app.use(ArcoVueIcon);
app.use(QRCode);

const IconFont = Icon.addFromIconFontCn({
  src: "https://at.alicdn.com/t/c/font_4594700_5nsjgjptkdn.js",
});
app.component("icon-font", IconFont);
app.component("page-header", PageHeader);
app.component("copy-text", CopyText);
app.component("input-text", InputText);
app.component("authorize", Authorize);
app.component("verify", Verify);
app.component("monaco-editor", MonacoEditor);

app.mount("#app");