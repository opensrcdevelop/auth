# Coding Conventions

**Analysis Date:** 2026-03-19

## Naming Patterns

### Files

**Java:**
- Entity: `User.java`, `Role.java`, `Permission.java`
- DTO: `UserRequestDto.java`, `UserResponseDto.java`
- Service Interface: `UserService.java`
- Service Implementation: `UserServiceImpl.java`
- Controller: `UserController.java`
- Mapper: `UserMapper.java`, `UserMapper.xml`
- Repository: `UserRepository.java`

**TypeScript/Vue:**
- Component: `CopyText.vue`, `UserAttrList.vue`
- API: `user.ts`, `role.ts`
- Util: `apiRequest.ts`

### Functions

**Java:**
- Method names use camelCase: `createUser()`, `getUserInfo()`, `listUsers()`
- Service methods: descriptive action verbs like `createUser()`, `updateUser()`, `removeUser()`, `getUserInfo()`
- Private helper methods: use descriptive names like `checkUsername()`, `validatePassword()`

**TypeScript:**
- Function names use camelCase: `getUserList()`, `updateUserAttr()`
- Export functions match API endpoints: `getUserDetail()`, `createUser()`

### Variables

**Java:**
- Local variables: camelCase `userId`, `requestDto`, `queryWrapper`
- Constants: UPPER_SNAKE_CASE `ACCOUNT_NOT_EXISTS`, `DEFAULT_PAGE_SIZE`
- Private static final for constant strings in services

**TypeScript:**
- Variables: camelCase `userList`, `params`, `formData`
- Props: camelCase with type definition

### Types

**Java:**
- Entities: singular nouns, `User`, `Role`, `Permission`
- DTOs: `UserRequestDto`, `UserResponseDto`, descriptive suffixes
- Enums: PascalCase, `UserStatus`, `PrincipalTypeEnum`
- Interfaces: `UserService`, `UserDetails`

**TypeScript:**
- Interfaces: PascalCase `UserInfo`, `RoleInfo`
- Types: PascalCase

## Code Style

### Formatting

**Java (Spotless + Eclipse JDT):**
- Tool: `com.diffplug.spotless` version 7.0.2
- Config: `config/spotless/eclipse-java-style.xml`
- Line length: 120 characters
- Indentation: 4 spaces (not tabs)
- Use spaces for tabulation

**Run commands:**
```bash
./gradlew spotlessApply      # Format code
./gradlew spotlessCheck     # Check formatting
```

**TypeScript/Vue:**
- Uses Vite for build
- TypeScript version: ~5.8.3
- Vue 3 with Composition API (`<script setup lang="ts">`)
- SCSS for styling

### Linting

**Java:**
- Integrated via Spotless
- Rules: remove unused imports, trim trailing whitespace, end with newline
- Import ordering: default (no custom order)

**TypeScript:**
- Uses `vue-tsc` for type checking
- No ESLint config found in project root (uses node_modules configs)

## Import Organization

### Java

Order:
1. External libraries (`java.*`, `javax.*`, `org.*`, `com.*`)
2. Spring framework (`org.springframework.*`)
3. Project packages (`cn.opensrcdevelop.*`)

Example:
```java
import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import cn.opensrcdevelop.auth.biz.dto.user.UserRequestDto;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.common.response.PageData;
```

### TypeScript

Order:
1. Vue/External imports
2. Project imports with `@/` alias

Example:
```typescript
import { ref } from "vue";
import apiRequest from "@/util/apiRequest";
import { getUserList } from "@/api/user";
```

### Path Aliases

- `@/` maps to `ui/src/`

## Error Handling

### Java

**Business Exceptions:**
- Use `BizException` from `cn.opensrcdevelop.common.exception`
- Throw with message code: `throw new BizException("USER_NOT_FOUND")`
- Message codes are strings for i18n support

**Exception Classes:**
```java
// Business exception
throw new BizException("账号不存在");

// With parameters
throw new BizException("USER_ALREADY_EXISTS", username);

// With cause
throw new BizException(e, "ERROR_MESSAGE");
```

**Validation:**
- Use Jakarta Bean Validation (`jakarta.validation`)
- DTOs use `@Validated` with `ValidationGroups`
- Groups: `ValidationGroups.Operation.INSERT.class`, `ValidationGroups.Operation.UPDATE.class`

**Controller Level:**
```java
@PostMapping
@Authorize({"allUserPermissions", "createUser"})
public void createUser(
        @RequestBody @Validated({ValidationGroups.Operation.INSERT.class}) UserRequestDto requestDto) {
    userService.createUser(requestDto);
}
```

**Global Exception Handling:**
- Custom handlers in `auth-server/src/main/java/cn/opensrcdevelop/auth/handler/`
- `ResourceAuthenticationExceptionHandler` for OAuth2 errors

### TypeScript

- No explicit error handling patterns observed
- API errors handled via axios interceptors in `apiRequest`

## Logging

**Framework:** SLF4J (not directly used in service code)

**Current Status:**
- Logging is NOT commonly used in service classes
- Minimal logging found in async task executors and Excel services
- For new code: use SLF4J with `@Slf4j` (Lombok) if logging is needed

**Logging Pattern:**
```java
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class UserServiceImpl {
    // Use: log.info(), log.error(), log.debug()
}
```

## Comments

### When to Comment

**Java:**
- Javadoc for public methods and classes: describe purpose, params, return values
- Inline comments for complex business logic
- Use Chinese language (project uses Chinese throughout)

**Example:**
```java
/**
 * 创建用户
 *
 * @param requestDto 创建用户请求
 */
@Audit(...)
@Transactional
@Override
public void createUser(UserRequestDto requestDto) {
    // 1. 检查用户名是否存在
    checkUsername(requestDto, null);
    // ...
}
```

**TypeScript/Vue:**
- JSDoc for exported functions
- Comments in Chinese for complex logic

### JSDoc/TSDoc

**TypeScript:**
```typescript
/**
 * 获取用户列表
 *
 * @param params 参数
 * @param data 请求体
 * @returns 用户列表
 */
export function getUserList(params: any, data: any = []) {
  return apiRequest.post({...});
}
```

## Function Design

### Size

**Java:**
- Methods should be under 50 lines (per quality gate)
- Use private helper methods for complex logic
- Service methods typically 20-40 lines

**TypeScript:**
- Vue methods should be concise
- Complex logic moved to composables or utils

### Parameters

**Java:**
- Use DTOs for multiple parameters: `UserRequestDto requestDto`
- Use primitive types for simple params: `String userId`, `int page`
- Avoid more than 4 parameters; use DTOs instead

**TypeScript:**
- Use typed objects for params
- Optional params with defaults

### Return Values

**Java:**
- Services return: `void`, entities, DTOs, `PageData<T>`, `List<T>`
- Controllers typically return entity/DTO directly (wrapped by `@RestResponse`)
- Use `Optional<T>` for nullable returns

**TypeScript:**
- Return promises for async calls
- Use typed returns for API functions

## Module Design

### Exports

**Java:**
- Public classes: Controller, Service interfaces, DTOs, Entities
- Package-private: implementation classes, mappers
- Use `@Component`, `@Service`, `@Repository` annotations

**TypeScript:**
- Named exports for API functions
- Default exports for Vue components

### Barrel Files

**Not Used:**
- No `index.ts` barrel files found
- Direct imports from files

### Package Structure

**auth-biz module:**
```
cn.opensrcdevelop.auth.biz/
├── constants/          # Constants and enums
├── component/          # Component classes
├── dto/                # Data transfer objects
│   ├── user/
│   ├── role/
│   └── ...
├── entity/             # Database entities
│   ├── user/
│   ├── role/
│   └── ...
├── mapper/             # MyBatis mappers
├── repository/         # Repository pattern
├── service/            # Business logic
│   ├── user/
│   │   ├── UserService.java
│   │   └── impl/
│   └── ...
└── util/               # Utility classes
```

## Controller Patterns

### REST Endpoints

```java
@Tag(name = "API-User", description = "接口-用户管理")
@RestController
@RestResponse
@RequestMapping("/user")
@RequiredArgsConstructor
public class UserController {

    @Operation(summary = "创建用户", description = "创建用户")
    @PostMapping
    @Authorize({"allUserPermissions", "createUser"})
    public void createUser(
            @RequestBody @Validated({ValidationGroups.Operation.INSERT.class}) UserRequestDto requestDto) {
        userService.createUser(requestDto);
    }
}
```

### Annotations

- `@Tag`, `@Operation`, `@Parameters`: OpenAPI/Swagger documentation
- `@Authorize`: Method-level authorization (from auth-client-starter)
- `@NoRestResponse`: Skip response wrapper for file downloads
- `@RestResponse`: Auto-wrap responses (global)

## Entity Patterns

### MyBatis-Plus Entities

```java
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_user")
@EntityName("用户")
public class User extends BaseEntity implements UserDetails, OAuth2User, Serializable {

    @TableId(type = IdType.INPUT)
    private String userId;

    private String username;

    @JsonIgnore
    private String password;

    @TableField(exist = false)
    private List<Role> roles;
}
```

### Annotations

- `@Data`: Lombok for getters/setters
- `@TableName`: Database table name
- `@TableId`: Primary key configuration
- `@TableField(exist = false)`: Non-database fields
- `@EntityName`: Audit annotation for Javers

## Validation Patterns

### DTO Validation

```java
public class UserRequestDto {

    @NotBlank(message = "用户名不能为空")
    private String username;

    @NotBlank(message = "密码不能为空")
    @Size(min = 8, max = 16, message = "密码长度需在 8-16 位之间")
    private String password;

    private String phoneNumber;

    private String emailAddress;
}
```

### Validation Groups

```java
public class ValidationGroups {
    public interface Operation {
        interface INSERT {}
        interface UPDATE {}
    }
}
```

---

*Convention analysis: 2026-03-19*
