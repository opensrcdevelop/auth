import { defineConfig, devices } from "@playwright/test";

/**
 * Playwright 配置文件
 * 用于用户导入导出功能的端到端测试
 */
export default defineConfig({
  testDir: "./e2e",
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1,
  reporter: [["html"], ["list"]],
  use: {
    baseURL: "http://auth.local.opensrcdevelop.cn:4321",
    trace: "on-first-retry",
    screenshot: "only-on-failure",
    video: "retain-on-failure",
  },
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],
  webServer: {
    command: "npm run dev",
    url: "http://auth.local.opensrcdevelop.cn:4321",
    reuseExistingServer: true,
    timeout: 120000,
  },
});
