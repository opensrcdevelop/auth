/**
 * 创建 E2E 测试用的 Excel 文件
 *
 * 使用方法：
 * npm run test:fixtures
 */

import ExcelJS from "exceljs";
import path from "path";
import fs from "fs";
import { fileURLToPath } from "url";

// ES 模块中获取 __dirname 的替代方案
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

/**
 * 创建有效的导入测试文件
 * 包含一个添加用户操作，数据完整且有效
 */
async function createValidImportFile(): Promise<void> {
  const workbook = new ExcelJS.Workbook();
  const worksheet = workbook.addWorksheet("用户导入模版");

  // 设置表头（根据实际模版格式）
  worksheet.columns = [
    { header: "操作类型", key: "operationType", width: 15 },
    { header: "用户名", key: "username", width: 20 },
    { header: "邮箱", key: "emailAddress", width: 30 },
    { header: "手机号", key: "phoneNumber", width: 20 },
    { header: "禁用", key: "locked", width: 10 },
    { header: "控制台访问", key: "consoleAccess", width: 15 },
    { header: "启用MFA", key: "enableMfa", width: 15 },
  ];

  // 添加示例数据行 - 添加用户
  worksheet.addRow({
    operationType: 0, // 0: 添加
    username: `testuser_${Date.now()}`,
    emailAddress: "test@example.com",
    phoneNumber: "13800138000",
    locked: "否",
    consoleAccess: "是",
    enableMfa: "否",
  });

  // 保存文件
  const filePath = path.join(__dirname, "valid-import.xlsx");
  await workbook.xlsx.writeFile(filePath);
  console.log(`已创建有效导入测试文件: ${filePath}`);
}

/**
 * 创建无效的导入测试文件
 * 包含错误数据，用于测试错误处理
 */
async function createInvalidImportFile(): Promise<void> {
  const workbook = new ExcelJS.Workbook();
  const worksheet = workbook.addWorksheet("用户导入模版");

  // 设置表头
  worksheet.columns = [
    { header: "操作类型", key: "operationType", width: 15 },
    { header: "用户名", key: "username", width: 20 },
    { header: "邮箱", key: "emailAddress", width: 30 },
    { header: "手机号", key: "phoneNumber", width: 20 },
    { header: "禁用", key: "locked", width: 10 },
    { header: "控制台访问", key: "consoleAccess", width: 15 },
    { header: "启用MFA", key: "enableMfa", width: 15 },
  ];

  // 添加错误数据行

  // 第 2 行：缺少必填的用户名
  worksheet.addRow({
    operationType: 0,
    username: "", // 空用户名
    emailAddress: "missing-username@example.com",
    phoneNumber: "13800138001",
    locked: "否",
    consoleAccess: "是",
    enableMfa: "否",
  });

  // 第 3 行：缺少必填的邮箱
  worksheet.addRow({
    operationType: 0,
    username: "missing-email-user",
    emailAddress: "", // 空邮箱
    phoneNumber: "13800138002",
    locked: "否",
    consoleAccess: "是",
    enableMfa: "否",
  });

  // 第 4 行：无效的操作类型
  worksheet.addRow({
    operationType: 999, // 无效的操作类型
    username: "invalid-operation",
    emailAddress: "invalid@example.com",
    phoneNumber: "13800138003",
    locked: "否",
    consoleAccess: "是",
    enableMfa: "否",
  });

  // 保存文件
  const filePath = path.join(__dirname, "invalid-import.xlsx");
  await workbook.xlsx.writeFile(filePath);
  console.log(`已创建无效导入测试文件: ${filePath}`);
}

/**
 * 主函数
 */
async function main(): Promise<void> {
  // 确保 fixtures 目录存在
  const fixturesDir = __dirname;
  if (!fs.existsSync(fixturesDir)) {
    fs.mkdirSync(fixturesDir, { recursive: true });
  }

  // 创建测试文件
  await createValidImportFile();
  await createInvalidImportFile();

  console.log("\n所有测试文件创建完成！");
}

// 运行
main().catch((error) => {
  console.error("创建测试文件失败:", error);
  process.exit(1);
});
