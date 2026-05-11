---
paths:
  - "auth-server/src/**/*.java"
  - "auth-biz/src/**/*.java"
  - "common/src/**/*.java"
  - "multi-tenant/src/**/*.java"
  - "auth-audit/src/**/*.java"
  - "ai-chatbi/src/**/*.java"
  - "auth-client-spring-boot-starter/src/**/*.java"
---

# Java 代码规范

本文档是 `quality-gate.md` 的补充，专门针对 Java 代码编写规范。

## 导入语句规范

### 禁止使用全限定类名

**规则**：在 Java 代码中，**必须**使用 `import` 语句导入类，**禁止**使用全限定类名（Fully Qualified Name）。

**原因**：
- 全限定类名使代码冗长、难以阅读
- 代码中充斥包路径，降低可读性
- 维护困难，包路径变更时需要逐一替换

**错误示例**：
```java
public class UserService {
    private java.util.List<cn.opensrcdevelop.auth.entity.User> users;

    public java.util.Optional<cn.opensrcdevelop.auth.entity.User> findById(Long id) {
        return userRepository.findById(id);
    }
}
```

**正确示例**：
```java
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

### 例外情况

以下情况允许使用全限定类名或静态导入：

1. **静态导入常量**：
   ```java
   import static org.junit.Assert.*;
   import static java.math.RoundingMode.HALF_UP;
   ```

2. **同包内类名冲突**：不同包中存在相同类名且需要同时使用时
   ```java
   // 同时使用 java.sql.Date 和 java.util.Date
   java.sql.Date sqlDate = new java.sql.Date(System.currentTimeMillis());
   ```

3. **注解中声明属性**：
   ```java
   @RequestMapping(value = "/api", method = RequestMethod.GET)
   ```

4. **反射相关的 Class 获取**：
   ```java
   Class<?> clazz = java.lang.Class.forName("com.example.Foo");
   ```

5. **方法返回值或参数需要明确类型时**（仅限泛型或 lambda 场景）
   ```java
   // 某些复杂的泛型链式调用可能需要明确类型
   Function<String, java.util.Optional<Integer>> parser = s -> java.util.Optional.of(Integer.parseInt(s));
   ```

## 常用 import 分组规范

建议的 import 分组顺序（通过 Spotless 的 `importOrder()` 自动处理）：

1. `java.*` - Java 标准库
2. `javax.*` - Java 扩展库
3. 第三方库（`org.*`、`com.*` 等）
4. 项目内部（`cn.*`、`com.example.*` 等）

## 类型使用规范

### 禁止使用原始类型（Raw Type）

**错误示例**：
```java
List users = new ArrayList();
Map map = new HashMap();
```

**正确示例**：
```java
List<User> users = new ArrayList<>();
Map<String, Object> map = new HashMap<>();
```

### 优先使用接口类型声明变量

**错误示例**：
```java
ArrayList<User> users = new ArrayList<>();
HashMap<String, Object> config = new HashMap<>();
```

**正确示例**：
```java
List<User> users = new ArrayList<>();
Map<String, Object> config = new HashMap<>();
```

## 方法设计规范

### 方法长度限制

**规则**：单个方法不超过 **50 行**

**原因**：
- 短方法更易理解、测试和维护
- 促进代码复用
- 便于 IDE 重构

**建议**：
- 如果方法超过 30 行，考虑是否可以拆分成更小的方法
- 提取重复代码到私有方法
- 提取条件判断到有明确命名的方法

### 方法命名规范

- **动词或动词短语**：`getUserById`、`saveOrder`、`deleteById`
- **布尔值方法**：`isActive`、`hasPermission`、`canAccess`
- **避免缩写**：`calculateTotalAmount` 而非 `calcTot`
- **避免单字母**（循环变量除外）：`processItem` 而非 `process`

## 类设计规范

### 类长度限制

**规则**：单个类不超过 **500 行**

**原因**：
- 大类难以维护和理解
- 暗示单一职责原则被违反
- 影响代码的可测试性

### 类的职责

- 一个类应该只有一个变更原因（单一职责原则）
- 优先使用组合而非继承
- 接口应该小而精确（接口隔离原则）

## 日志规范

### 必须使用 SLF4J

**错误示例**：
```java
System.out.println("User created: " + userId);
e.printStackTrace();
```

**正确示例**：
```java
log.info("User created: {}", userId);
log.error("Failed to create user", e);
```

### 日志级别使用

| 级别 | 使用场景 |
|------|----------|
| `ERROR` | 错误异常，影响功能 |
| `WARN` | 潜在问题，如配置缺失使用默认值 |
| `INFO` | 重要业务事件（登录、登出、操作） |
| `DEBUG` | 开发调试信息，生产环境关闭 |
| `TRACE` | 详细调试信息，生产环境关闭 |

## 异常处理规范

### 禁止空的 catch 块

**错误示例**：
```java
try {
    doSomething();
} catch (Exception e) {
    // 什么都不做
}
```

**正确示例**：
```java
try {
    doSomething();
} catch (SpecificException e) {
    log.warn("Expected exception during operation", e);
    // 或重新抛出
    throw new BusinessException("Operation failed", e);
}
```

### 不要使用异常控制流程

**错误示例**：
```java
try {
    user = findUser();
} catch (UserNotFoundException e) {
    user = createDefaultUser();
}
```

**正确示例**：
```java
Optional<User> userOpt = findUserOptional();
user = userOpt.orElseGet(this::createDefaultUser);
```

## 泛型规范

### 使用泛型方法而非强制类型转换

**错误示例**：
```java
List list = getUsers();
String name = (String) list.get(0);
```

**正确示例**：
```java
List<String> users = getUsers();
String name = users.get(0);
```

### 泛型通配符使用

- `List<?>` - 仅读取，不修改
- `List<? extends Number>` - 读取 Number 及其子类
- `List<? super Integer>` - 写入 Integer 及其父类

## Stream API 规范

### 链式调用格式化

```java
List<String> names = users.stream()
        .filter(User::isActive)
        .map(User::getName)
        .sorted()
        .collect(Collectors.toList());
```

### 避免过长链式调用

如果链式调用超过 3-4 行，考虑拆分或使用变量中转。

## 注释规范

### 禁止提交 TODO/FIXME

**规则**：代码中的 `TODO`、`FIXME` 必须创建 Issue 跟踪，代码提交时不应包含未解决的 TODO/FIXME。

**例外**：如果确实需要临时标记，明确添加说明：
```java
// TODO(username): 这是一个临时方案，需要在 XXX 后重构
void temporaryMethod() {
    // ...
}
```

### 注释要求

- 注释应该解释**为什么**，而不是**是什么**
- 公共 API 应有 Javadoc
- 复杂业务逻辑应添加解释性注释

## 依赖注入规范

### 优先使用构造器注入

**错误示例**：
```java
@Autowired
private UserService userService;
```

**正确示例**：
```java
private final UserService userService;

public UserController(UserService userService) {
    this.userService = userService;
}
```

## 常见错误对照表

| 错误写法 | 正确写法 |
|----------|----------|
| `java.time.LocalDateTime.now()` | `LocalDateTime.now()` + import |
| `java.lang.System.out.println()` | `log.info()` 或 `System.out.println()` (仅 main 方法) |
| `List list = new ArrayList()` | `List<T> list = new ArrayList<>()` |
| `public void method() throws Exception` | 具体异常类型 |
| `catch (Exception e) {}` | 具体异常 + 适当处理 |
| `if (condition) { return true; } else { return false; }` | `return condition;` |

## 检查方法

提交代码前，运行以下命令检查：

```bash
# 1. 检查是否使用了全限定类名（需要人工检查）
grep -rn "java\." --include="*.java" src/ | grep -v "import\|//\|javadoc"

# 2. 格式化代码
./gradlew spotlessApply

# 3. 编译检查
./gradlew build -x test
```