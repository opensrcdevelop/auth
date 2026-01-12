import { expect, test } from "@playwright/test";
import * as path from "path";
import * as fs from "fs";

/**
 * 用户导入导出功能 E2E 测试
 */
test.describe("用户导入导出功能", () => {
  // 测试前登录
  test.beforeEach(async ({ page }) => {
    // 导航到登录页面
    await page.goto("/ui/login");

    // 填写登录表单
    await page.fill('input[placeholder="请输入用户名"]', "admin");
    await page.fill('input[placeholder="请输入密码"]', "123456");

    // 点击登录按钮
    await page.click('button[type="submit"]');

    // 等待导航到用户管理页面
    await page.waitForURL("/ui/user");
  });

  /**
   * 测试下载模版功能
   */
  test("应该能够下载用户导入模版", async ({ page }) => {
    // 导航到用户管理页面
    await page.goto("/ui/user");

    // 等待页面加载
    await page.waitForSelector('text=用户列表');

    // 设置下载监听
    const downloadPromise = page.waitForEvent("download");

    // 点击下载模版按钮
    await page.click('button:has-text("下载模版")');

    // 等待下载完成
    const download = await downloadPromise;

    // 验证下载的文件名
    expect(download.suggestedFilename()).toBe("用户导入模版.xlsx");

    // 保存文件到临时目录
    const tempDir = path.join(__dirname, "temp");
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }
    const downloadPath = path.join(tempDir, download.suggestedFilename());
    await download.saveAs(downloadPath);

    // 验证文件存在且大小大于 0
    expect(fs.existsSync(downloadPath)).toBe(true);
    expect(fs.statSync(downloadPath).size).toBeGreaterThan(0);

    // 清理临时文件
    fs.unlinkSync(downloadPath);
  });

  /**
   * 测试导出当前页数据功能
   */
  test("应该能够导出当前页用户数据", async ({ page }) => {
    // 导航到用户管理页面
    await page.goto("/ui/user");

    // 等待用户列表加载
    await page.waitForSelector('text=用户列表');

    // 等待表格数据加载
    await page.waitForSelector(".arco-table");

    // 设置下载监听
    const downloadPromise = page.waitForEvent("download");

    // 点击导出当前页按钮
    await page.click('button:has-text("导出当前页")');

    // 等待下载完成
    const download = await downloadPromise;

    // 验证下载的文件名格式（用户数据_YYYY-MM-DD.xlsx）
    const filenamePattern = /^用户数据_\d{4}-\d{2}-\d{2}\.xlsx$/;
    expect(download.suggestedFilename()).toMatch(filenamePattern);

    // 保存文件到临时目录
    const tempDir = path.join(__dirname, "temp");
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }
    const downloadPath = path.join(tempDir, download.suggestedFilename());
    await download.saveAs(downloadPath);

    // 验证文件存在且大小大于 0
    expect(fs.existsSync(downloadPath)).toBe(true);
    expect(fs.statSync(downloadPath).size).toBeGreaterThan(0);

    // 清理临时文件
    fs.unlinkSync(downloadPath);
  });

  /**
   * 测试导出全部数据功能
   */
  test("应该能够导出全部用户数据", async ({ page }) => {
    // 导航到用户管理页面
    await page.goto("/ui/user");

    // 等待用户列表加载
    await page.waitForSelector('text=用户列表');

    // 设置下载监听
    const downloadPromise = page.waitForEvent("download");

    // 点击导出全部按钮
    await page.click('button:has-text("导出全部")');

    // 等待下载完成
    const download = await downloadPromise;

    // 验证下载的文件名格式
    const filenamePattern = /^用户数据_\d{4}-\d{2}-\d{2}\.xlsx$/;
    expect(download.suggestedFilename()).toMatch(filenamePattern);

    // 保存文件到临时目录
    const tempDir = path.join(__dirname, "temp");
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }
    const downloadPath = path.join(tempDir, download.suggestedFilename());
    await download.saveAs(downloadPath);

    // 验证文件存在且大小大于 0
    expect(fs.existsSync(downloadPath)).toBe(true);
    expect(fs.statSync(downloadPath).size).toBeGreaterThan(0);

    // 清理临时文件
    fs.unlinkSync(downloadPath);
  });

  /**
   * 测试导入功能 - 成功场景
   */
  test("应该能够成功导入用户数据", async ({ page }) => {
    // 导航到用户管理页面
    await page.goto("/ui/user");

    // 等待用户列表加载
    await page.waitForSelector('text=用户列表');

    // 创建测试用的 Excel 文件路径
    const testFilePath = path.join(__dirname, "fixtures", "valid-import.xlsx");

    // 确保测试文件存在
    if (!fs.existsSync(testFilePath)) {
      test.skip();
      return;
    }

    // 设置文件输入
    const fileInput = page.locator('input[type="file"]');
    await fileInput.setInputFiles(testFilePath);

    // 等待导入结果对话框出现
    await page.waitForSelector(".arco-modal");

    // 验证导入结果对话框显示
    await expect(page.locator(".arco-modal")).toBeVisible();

    // 验证成功消息
    await expect(page.locator("text=导入完成")).toBeVisible();

    // 点击确定关闭对话框
    await page.click('.arco-modal .arco-btn-primary');

    // 等待对话框关闭
    await expect(page.locator(".arco-modal")).not.toBeVisible();
  });

  /**
   * 测试导入功能 - 失败场景
   */
  test("应该能够显示导入错误信息", async ({ page }) => {
    // 导航到用户管理页面
    await page.goto("/ui/user");

    // 等待用户列表加载
    await page.waitForSelector('text=用户列表');

    // 创建包含错误的测试用 Excel 文件路径
    const testFilePath = path.join(__dirname, "fixtures", "invalid-import.xlsx");

    // 确保测试文件存在
    if (!fs.existsSync(testFilePath)) {
      test.skip();
      return;
    }

    // 设置文件输入
    const fileInput = page.locator('input[type="file"]');
    await fileInput.setInputFiles(testFilePath);

    // 等待导入结果对话框出现
    await page.waitForSelector(".arco-modal");

    // 验证导入结果对话框显示
    await expect(page.locator(".arco-modal")).toBeVisible();

    // 验证警告状态（有失败记录）
    await expect(page.locator(".arco-result-status-warning")).toBeVisible();

    // 验证错误表格显示
    await expect(page.locator(".arco-table")).toBeVisible();

    // 点击确定关闭对话框
    await page.click('.arco-modal .arco-btn-primary');

    // 等待对话框关闭
    await expect(page.locator(".arco-modal")).not.toBeVisible();
  });

  /**
   * 测试用户列表页面基本功能
   */
  test("用户列表页面应该正常显示", async ({ page }) => {
    // 导航到用户管理页面
    await page.goto("/ui/user");

    // 等待页面加载
    await page.waitForSelector('text=用户列表');

    // 验证页面标题
    await expect(page.locator("text=用户列表")).toBeVisible();
    await expect(page.locator("text=对用户进行统一管理")).toBeVisible();

    // 验证操作按钮存在
    await expect(page.locator('button:has-text("下载模版")')).toBeVisible();
    await expect(page.locator('button:has-text("导出当前页")')).toBeVisible();
    await expect(page.locator('button:has-text("导出全部")')).toBeVisible();
    await expect(page.locator('button:has-text("导入数据")')).toBeVisible();
    await expect(page.locator('button:has-text("创建用户")')).toBeVisible();

    // 验证搜索框存在
    await expect(page.locator('input[placeholder="输入用户名进行搜索"]')).toBeVisible();

    // 验证表格存在
    await expect(page.locator(".arco-table")).toBeVisible();
  });

  /**
   * 测试搜索功能
   */
  test("应该能够搜索用户", async ({ page }) => {
    // 导航到用户管理页面
    await page.goto("/ui/user");

    // 等待页面加载
    await page.waitForSelector('text=用户列表');

    // 等待表格数据加载
    await page.waitForSelector(".arco-table");

    // 获取初始行数
    const initialRows = await page.locator(".arco-table-tr").count();

    // 输入搜索关键词
    await page.fill('input[placeholder="输入用户名进行搜索"]', "admin");

    // 按回车搜索
    await page.keyboard.press("Enter");

    // 等待搜索结果
    await page.waitForTimeout(500);

    // 验证搜索结果（可能行数减少）
    const searchRows = await page.locator(".arco-table-tr").count();
    expect(searchRows).toBeLessThanOrEqual(initialRows);

    // 清空搜索
    await page.click('input[placeholder="输入用户名进行搜索"] + .arco-input-suffix .arco-icon-close');

    // 等待列表恢复
    await page.waitForTimeout(500);
  });
});
