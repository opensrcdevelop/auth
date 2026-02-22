import { defineConfig, loadEnv } from "vite";
import vue from "@vitejs/plugin-vue";
import path from "path";
import fs from "fs";
import os from "os";
import http from "http";

// https://vitejs.dev/config/
export default ({ mode }) => {
  const env = loadEnv(mode, process.cwd());

  // SSL 证书路径（用户目录）
  const sslDir = path.join(os.homedir(), ".ssl", "auth");
  const httpsEnabled = env.VITE_HTTPS === "true";

  // HTTPS 配置
  let https: { key: fs.ReadBuffer; cert: fs.ReadBuffer } | undefined;
  if (httpsEnabled) {
    const keyFile = path.join(sslDir, "key.pem");
    const certFile = path.join(sslDir, "cert.pem");
    if (fs.existsSync(keyFile) && fs.existsSync(certFile)) {
      https = {
        key: fs.readFileSync(keyFile),
        cert: fs.readFileSync(certFile),
      };
      console.log("[Vite] HTTPS enabled using certificates from:", sslDir);
    } else {
      console.warn("[Vite] HTTPS enabled but certificates not found at:", sslDir);
    }
  }

  // HTTP 到 HTTPS 重定向插件
  const httpRedirectPlugin = () => ({
    name: "http-redirect-to-https",
    configureServer(server: any) {
      if (!httpsEnabled) return;

      // 创建 HTTP 服务器监听 80 端口
      const httpServer = http.createServer((req, res) => {
        const host = req.headers.host?.split(":")[0] || "localhost";
        const httpsPort = 4321;
        res.writeHead(301, {
          Location: `https://${host}:${httpsPort}${req.url}`,
        });
        res.end();
      });

      httpServer.listen(80, () => {
        console.log("[Vite] HTTP server redirecting to HTTPS on port 80");
      });

      // 清理
      process.on("SIGTERM", () => httpServer.close());
      process.on("SIGINT", () => httpServer.close());
    },
  });

  return defineConfig({
    base: env.VITE_UI_BASE_PATH || "/",
    plugins: [vue(), httpRedirectPlugin()],
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
      allowedHosts: ["auth.local.opensrcdevelop.cn", "localhost", "127.0.0.1", ".local", "all"],
      https,
    },
  });
};
