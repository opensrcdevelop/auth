# E2E 测试使用说明

## 前置条件

### 1. 配置 hosts 文件

确保 `/etc/hosts` 中包含以下配置：
```
127.0.0.1  auth.local.opensrcdevelop.cn
```

### 2. 启动后端服务

在项目根目录下运行：
```bash
./gradlew :auth-server:bootRun
```

后端服务将运行在 `http://auth.local.opensrcdevelop.cn:6543`

### 3. 确保 admin 用户存在

测试使用以下凭据登录：
- 用户名: `admin`
- 密码: `123456`

## 运行测试

### 方式一：自动启动前端服务（推荐）

Playwright 配置已设置为自动启动前端开发服务器：

```bash
# 运行所有 E2E 测试
npm run test:e2e

# 使用 UI 模式运行（推荐用于调试）
npm run test:e2e:ui

# 使用调试模式运行
npm run test:e2e:debug
```

### 方式二：手动启动前端服务

如果需要手动控制前端服务：

1. 启动前端开发服务器：
```bash
npm run dev
```

2. 修改 `playwright.config.ts`，禁用自动启动：
```typescript
webServer: {
  command: "npm run dev",
  url: "http://localhost:4322",
  reuseExistingServer: true,  // 改为 true，使用已运行的服务
  timeout: 120000,
}
```

3. 运行测试：
```bash
npm run test:e2e
```

## 测试文件

- `user-import-export.spec.ts` - 用户导入导出功能测试

## 测试 Fixture 文件

测试所需的 Excel 文件位于 `fixtures/` 目录：

- `valid-import.xlsx` - 有效的导入测试文件
- `invalid-import.xlsx` - 包含错误的测试文件

如果需要重新生成这些文件：
```bash
npm run test:fixtures
```

## 测试报告

测试运行后，HTML 报告将生成在 `playwright-report/` 目录中：

```bash
# 打开测试报告
npx playwright show-report
```

## 故障排查

### 问题：连接被拒绝

确保后端服务正在运行：
```bash
curl http://auth.local.opensrcdevelop.cn:6543/health
```

### 问题：登录失败

确保 admin 用户存在且密码正确。

### 问题：测试超时

增加 `playwright.config.ts` 中的超时时间：
```typescript
use: {
  actionTimeout: 30000,  // 增加操作超时时间
  navigationTimeout: 60000,  // 增加导航超时时间
}
```

## 添加新测试

1. 在 `e2e/` 目录下创建新的测试文件（如 `new-test.spec.ts`）
2. 使用 Playwright API 编写测试
3. 运行测试验证

## CI/CD 集成

在 CI 环境中运行测试时，需要：

1. 安装 Playwright 浏览器：`npm run test:e2e:install`
2. 确保后端服务可用
3. 运行测试：`CI=true npm run test:e2e`
