# 任务进度记录规范

## 任务记录目录结构

```
.claude/tmp/tasks/
└── YYYY-MM-DD/           # 当天日期文件夹
    └── task-name.md      # 任务记录文件
```

## 任务记录文件模板

每个任务的 md 文件必须包含以下内容：

```markdown
# [任务名称]

## 基本信息
- **创建时间**: YYYY-MM-DD HH:mm
- **任务状态**: 进行中 | 已完成 | 已暂停

## 任务计划
<!-- 详细描述任务目标和计划 -->

### 计划进度
- [ ] 子任务 1
- [ ] 子任务 2
- [ ] 子任务 3

## 完成情况
<!-- 记录实际完成的工作内容和进度更新 -->

## 文件变更记录

### 创建的文件
- `/绝对路径/文件1.ext`
- `/绝对路径/文件2.ext`

### 修改的文件
- `/绝对路径/文件3.ext`
- `/绝对路径/文件4.ext`

### 删除的文件
- `/绝对路径/文件5.ext`

## 备注
<!-- 其他需要记录的信息 -->
```

## 执行规则

### 1. 开始任务时
1. 创建当天日期文件夹（如不存在）：`.claude/tmp/tasks/YYYY-MM-DD/`
2. 创建任务记录文件：`任务名称.md`
3. 填写任务计划、计划进度等基本信息

### 2. 执行任务过程中
- 每完成一个子任务，更新计划进度中的复选框
- 记录创建、修改、删除的文件（使用绝对路径）
- 定期更新完成情况

### 3. 任务完成后
- 更新任务状态为"已完成"
- 确认所有文件变更已记录
- 填写最终完成情况

### 4. 文件路径格式
- **必须使用绝对路径**，例如：`/Users/lee0407/dev/projs/auth/auth-server/src/main/java/...`
- 不使用相对路径或 `~` 符号

## 示例

```markdown
# 实现用户登录功能

## 基本信息
- **创建时间**: 2026-01-10 15:30
- **任务状态**: 进行中

## 任务计划
实现基于用户名密码的用户登录功能，包括图形验证码校验。

### 计划进度
- [x] 创建登录控制器
- [ ] 实现登录业务逻辑
- [ ] 添加图形验证码
- [ ] 编写单元测试

## 完成情况
已完成登录控制器的基础框架，已定义 API 端点。

## 文件变更记录

### 创建的文件
- `/Users/lee0407/dev/projs/auth/auth-server/src/main/java/cn/opensrcdevelop/auth/controller/LoginController.java`
- `/Users/lee0407/dev/projs/auth/auth-server/src/main/resources/mapper/UserMapper.xml`

### 修改的文件
- `/Users/lee0407/dev/projs/auth/auth-server/src/main/resources/application.yml`
- `/Users/lee0407/dev/projs/auth/auth-biz/src/main/java/cn/opensrcdevelop/auth/service/LoginService.java`

### 删除的文件
- (无)

## 备注
需要参考 Spring Security 官方文档进行认证配置。
```
