import { defineConfig, loadEnv } from "vite";
import vue from "@vitejs/plugin-vue";
import path from "path";

// https://vitejs.dev/config/
export default ({ mode }) => {
  const env = loadEnv(mode, process.cwd());

  return defineConfig({
    base: env.VITE_UI_BASE_PATH || "/",
    plugins: [vue()],
    build: {
      outDir: "../auth-server/src/main/resources/ui",
    },
    resolve: {
      alias: {
        // src 别名配置
        "@": path.resolve(__dirname, "src"),
      },
    },
    server: {
      host: "0.0.0.0",
      port: 4321,
      open: false,
      strictPort: true,
      allowedHosts: ["auth.local.opensrcdevelop.cn", "localhost", ".local"],
    },
  });
};
