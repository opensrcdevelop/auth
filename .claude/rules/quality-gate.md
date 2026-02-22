# 质量红线规则

**重要：以下规则是绝对底线，没有任何例外情况**

## 提交前强制检查（必须全部通过）

在提交任何代码之前，必须确保以下检查全部通过：

### 1. 后端代码检查（Java）

#### 格式化检查
```bash
# 格式化代码
./gradlew spotlessApply

# 检查格式化
./gradlew spotlessCheck
```
**标准**：零警告、零错误

#### 编译检查
```bash
./gradlew build -x test
```
**标准**：编译成功，无错误

#### 测试检查
```bash
./gradlew test
```
**标准**：所有测试必须通过

### 2. 前端代码检查（Vue 3 + TypeScript）

#### 类型检查
```bash
cd ui && npm run type-check
```
**标准**：TypeScript 严格模式下无错误

#### 构建检查
```bash
cd ui && npm run build
```
**标准**：构建成功，无错误

### 3. 全量构建验证
```bash
./gradlew clean build -x test
```
**标准**：所有模块构建成功

---

## 绝对禁止清单（绝不允许）

以下行为在任何情况下都不允许：

### 代码质量

- **绝不**提交未通过格式化检查的代码（`spotlessCheck` 失败）
- **绝不**提交未通过类型检查的代码（`type-check` 失败）
- **绝不**提交编译失败的代码
- **绝不**使用 `TODO`、`FIXME` 作为最终代码（必须创建 Issue 跟踪）
- **绝不**跳过错误处理
- **绝不**吞掉异常（空 catch 块或仅打印日志）
- **绝不**使用魔法数字（必须定义常量）

### 测试代码规范

- **绝不**在编写业务代码后单独编写测试（必须同步编写）
- **绝不**提交无测试覆盖的业务代码
- **绝不**仅检查数量（如数据库记录数、文件数）作为断言
- **必须**验证数据内容是否准确（如数据库字段值、文件内容）
- **必须**验证边界条件和异常场景
- **必须**使用有意义的断言消息
- **测试方法命名**必须清晰描述被测试的场景

#### 测试断言要求（反例 vs 正例）

**❌ 错误示例：仅检查数量**
```java
// 仅验证插入了一条记录，未验证数据内容
assertEquals(1, userRepository.findAll().size());
```

**✅ 正确示例：验证完整数据**
```java
// 验证数据是否准确写入数据库
User savedUser = userRepository.findById(userId).orElseThrow();
assertEquals("expectedUsername", savedUser.getUsername());
assertEquals("expectedEmail", savedUser.getEmail());
assertEquals(UserStatus.ACTIVE, savedUser.getStatus());
assertNotNull(savedUser.getCreatedAt());
```

**❌ 错误示例：仅检查文件数量**
```java
// 仅验证文件存在，未验证内容
assertEquals(1, Files.list(outputDir).count());
```

**✅ 正确示例：验证文件内容**
```java
// 验证文件内容是否符合预期
Path outputFile = outputDir.resolve("export.xlsx");
assertTrue(Files.exists(outputFile));
assertArrayEquals(
    expectedFileBytes,
    Files.readAllBytes(outputFile)
);
// 或使用 EasyExcel 验证内容结构
List<UserData> actualData = EasyExcel.read(outputFile.toFile()).head(UserData.class).sheet().doReadSync();
assertEquals(expectedUsers, actualData);
```

**❌ 错误示例：仅检查方法调用**
```java
// 仅验证方法被调用，未验证参数和返回值
verify(userService).createUser(any());
```

**✅ 正确示例：验证完整行为**
```java
// 验证方法调用、参数和返回值
ArgumentCaptor<UserCreateRequest> captor = ArgumentCaptor.forClass(UserCreateRequest.class);
verify(userService).createUser(captor.capture());
assertEquals("expectedUsername", captor.getValue().getUsername());
verify(userService, times(1)).createUser(any());
assertTrue(result.isSuccess());
```

#### 测试覆盖场景

每个测试方法必须覆盖以下场景（如适用）：

1. **正常场景**：验证业务逻辑正确执行
2. **边界条件**：验证空值、空集合、最大/最小值等
3. **异常场景**：验证异常处理是否正确
4. **数据验证**：验证数据完整性和准确性

### Java 代码规范

- **绝不**使用全限定类名（必须使用 import 语句）
- **绝不**使用 `@SuppressWarnings` 绕过编译警告（除非有明确注释说明原因）
- **绝不**使用原始类型（Raw Type）
- **绝不**忽略 CheckStyle 或 Spotless 警告
- **绝不**在 finally 块中抛出异常
- **绝不**使用 `System.out.println` 或 `e.printStackTrace()`（必须使用 SLF4J）

#### 导入语句规范（反例 vs 正例）

**❌ 错误示例：使用全限定类名**
```java
// 糟糕：代码中充斥全限定类名，可读性差
public class UserService {
    private java.util.List<cn.opensrcdevelop.auth.entity.User> users;

    public java.util.Optional<cn.opensrcdevelop.auth.entity.User> findById(Long id) {
        return userRepository.findById(id);
    }
}
```

**✅ 正确示例：使用 import 语句**
```java
// 正确：通过 import 导入，代码简洁可读
import cn.opensrcdevelop.auth.entity.User;
import java.util.List;
import java.util.Optional;

public class UserService {
    private List<User> users;

    public Optional<User> findById(Long id) {
        return userRepository.findById(id);
    }
}
```

**唯一允许的例外**：
- 静态导入常量时（如 `import static org.junit.Assert.*;`）
- 不同包中存在相同类名且需要同时使用时（通过静态导入或别名处理）
- 注解中声明属性时（如 `@RequestMapping(value = "/api", method = RequestMethod.GET)`）

### TypeScript/Vue 代码规范

- **绝不**使用 `any` 类型（用 `unknown` 替代）
- **绝不**使用 `@ts-ignore` 或 `@ts-nocheck`
- **绝不**在模板中使用隐式 any 类型
- **绝不**忽略 TypeScript 编译错误

### 安全相关

- **绝不**硬编码密钥/凭证（必须使用环境变量或配置中心）
- **绝不**在日志中输出敏感信息（密码、token、身份证号等）
- **绝不**跳过输入验证
- **绝不**直接拼接 SQL（必须使用参数化查询或 MyBatis-Plus）
- **绝不**信任用户输入未经验证直接使用

### 代码规范

- **绝不**提交包含调试代码的提交（断点、console.log、debugger 等）
- **绝不**提交注释掉的代码块
- **绝不**提交无用的导入语句
- **绝不**使用拼音命名（类名、方法名、变量名）
- **绝不**创建上帝类（单个类超过 500 行）
- **绝不**创建超长方法（单个方法超过 50 行）

### Git 提交规范

- **绝不**提交空白信息的提交
- **绝不**向 main 或 develop 分支直接推送代码
- **绝不**在未验证功能的情况下提交代码
- **绝不**提交包含个人敏感信息的代码

---

## 检查清单

提交前必须逐项确认：

### 后端变更
- [ ] `./gradlew spotlessApply` 已执行
- [ ] `./gradlew spotlessCheck` 通过
- [ ] `./gradlew build -x test` 编译成功
- [ ] `./gradlew test` 测试通过（如有新增代码）
- [ ] 新增代码有对应的测试覆盖
- [ ] 测试断言验证了数据内容（而非仅数量）
- [ ] 测试覆盖了正常场景、边界条件和异常场景
- [ ] 无新增编译警告
- [ ] 无调试代码残留

### 前端变更
- [ ] `npm run type-check` 通过
- [ ] `npm run build` 构建成功
- [ ] 无 console.log 残留
- [ ] 无调试断点残留
- [ ] 组件已正确导出和类型声明

### 通用检查
- [ ] 无敏感信息泄露
- [ ] 代码已自我审查
- [ ] 功能已本地验证
- [ ] 提交信息符合规范

---

## 违反红线的处理

如果发现代码违反以上任何一条红线：

1. **必须立即修复**，不能拖延
2. **不能使用任何方式绕过**（如忽略规则、临时禁用检查等）
3. **不能以"临时方案"为由例外**
4. **CI/CD 检查失败时必须修复后才能合并**

---

## 项目特定规则

### 多租户相关
- **绝不**在不设置租户上下文的情况下直接操作数据库
- **绝不**在 SQL 中硬编码租户 ID
- **必须**使用 `@TenantLimit` 注解保护敏感接口

### 安全相关
- **绝不**在代码中硬编码密码
- **绝不**使用不安全的加密算法（如 MD5、SHA1）
- **必须**使用 `BCrypt` 或更强算法进行密码加密
- **必须**对用户输入进行验证和清理

### Spring Boot 相关
- **绝不**在 `@Autowired` 字段上注入（使用构造器注入）
- **绝不**使用 `@PostConstruct` 进行复杂初始化
- **绝不**在 Controller 中直接包含业务逻辑
- **必须**使用 `@Transactional` 明确事务边界

### MyBatis-Plus 相关
- **绝不**直接使用 `BaseMapper` 的危险方法（如 `delete`、`update` 无条件方法）
- **必须**使用 `LambdaQueryWrapper` 保证类型安全
- **绝不**在 XML 中直接拼接 SQL

### Vue 相关
- **绝不**在 template 中编写复杂逻辑（移到 computed 或 methods）
- **绝不**直接修改 props
- **必须**使用 defineComponent 或 `<script setup>`
- **必须**为组件定义 displayName
