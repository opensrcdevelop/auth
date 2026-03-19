# Testing Patterns

**Analysis Date:** 2026-03-19

## Test Framework

### Java Backend

**Runner:**
- JUnit 5 (JUnit Platform)
- Version: Managed by Spring Boot 3.5.9 (`spring-boot-starter-test`)
- Config: `build.gradle` (root level)

**Assertion Library:**
- AssertJ (`org.assertj:assertj-core`) version 3.8.0 (from Spring Boot BOM)
- JUnit Jupiter assertions (`org.junit.jupiter.api.Assertions`)

**Run Commands:**
```bash
./gradlew test                          # Run all tests
./gradlew test --tests UserServiceTest  # Run specific test class
./gradlew :auth-biz:test               # Run specific module tests
```

### Frontend

**E2E Testing:**
- Playwright (`@playwright/test`) version 1.49.1
- Config: Not found in project root

**Run Commands:**
```bash
cd ui && npm run test:e2e             # Run E2E tests
cd ui && npm run test:e2e:ui          # Run E2E tests with UI
cd ui && npm run test:e2e:debug       # Debug mode
```

## Test File Organization

### Location

**Java:**
- Test files: `src/test/java/` alongside main source
- Same package structure as main code

**Pattern:**
```
auth-biz/src/main/java/cn/opensrcdevelop/auth/biz/service/user/
├── UserService.java
└── impl/
    └── UserServiceImpl.java

auth-biz/src/test/java/cn/opensrcdevelop/auth/biz/service/user/
└── impl/
    └── UserServiceImplTest.java
```

**Naming:**
- Test classes: `{ClassName}Test` or `{ClassName}Tests`
- Example: `UserServiceImplTest`, `LengthPasswordRuleTests`

### Structure

**Common Test Modules:**
- `common/src/test/java/cn/opensrcdevelop/common/` - Unit tests for common utilities
- `auth-client-spring-boot-starter/src/test/java/` - Integration tests for client SDK
- `ai-chatbi/src/test/java/` - AI tool tests

## Test Structure

### Unit Test Pattern

```java
package cn.opensrcdevelop.common.srcurity.password.rule;

import static org.junit.jupiter.api.Assertions.assertEquals;

import cn.opensrcdevelop.common.security.password.rule.LengthPasswordRule;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class LengthPasswordRuleTests {

    private final LengthPasswordRule rule = new LengthPasswordRule(8, 16);

    @ParameterizedTest
    @MethodSource("provideTestCases")
    void testValidate(String password, boolean expected) {
        assertEquals(expected, rule.validate(password));
    }

    private static Stream<Arguments> provideTestCases() {
        return Stream.of(
                Arguments.of(null, false),
                Arguments.of("", false),
                Arguments.of("a", false),
                Arguments.of("1234567", false),
                Arguments.of("12345678", true),
                Arguments.of("1234567812345678", true),
                Arguments.of("12345678123456781", false));
    }

    @Test
    void testGetRuleName() {
        assertEquals("长度要求 8 位及以上，16 位及以下。", rule.getRuleName());
    }
}
```

### Patterns

**Setup:**
- Direct instantiation: `private final LengthPasswordRule rule = new LengthPasswordRule(8, 16);`
- For Spring beans: use `@SpringBootTest` or test configuration classes

**Teardown:**
- JUnit Jupiter handles cleanup automatically
- No explicit teardown patterns observed

**Assertion:**
- Use AssertJ-style assertions via JUnit Jupiter
- `assertEquals(expected, actual)`
- `assertNotNull()`, `assertNull()`
- `assertTrue()`, `assertFalse()`

## Mocking

### Framework

**Status:**
- Mockito is included via Spring Boot test starter
- No explicit mocking patterns found in existing tests
- Tests use direct instantiation for unit testing utility classes

### Common Patterns

**Direct Testing (Preferred for Pure Functions):**
```java
// Test the actual class directly
private final ExpressionEngine expressionEngine = new ExpressionEngine(1024, List.of(new TimeFunction()));

@Test
void evaluateTest1() {
    String expression = "fn_time:nowTime()";
    String result = (String) expressionEngine.evaluate(expression);
    assertNotNull(result);
}
```

**For Spring Beans (Not observed in codebase):**
```java
@MockBean
private UserService userService;

@InjectMocks
private UserController userController;
```

### What to Mock

- External services (if integration tests existed)
- Database repositories (if unit testing controllers)
- Time-sensitive operations with fixed clocks

### What NOT to Mock

- Pure utility classes and business logic
- Domain objects and DTOs
- The classes under test themselves

## Fixtures and Factories

### Test Data

**Pattern:**
- Inline test data in `@MethodSource` or test methods
- No external fixture files found

**Example:**
```java
private static Stream<Arguments> provideTestCases() {
    return Stream.of(
            Arguments.of(null, false),
            Arguments.of("", false),
            Arguments.of("12345678", true)
    );
}
```

### Location

- Test data defined within test methods or as static providers
- No separate fixture files

## Coverage

### Requirements

**Status:** Not enforced

**View Coverage:**
```bash
# No coverage command configured
# Consider adding:
./gradlew test --coverage
```

### Best Practices

Based on codebase analysis:
- Focus on testing business logic in `auth-biz` module
- Test password rules, expression engines, validation logic
- Controller tests would require Spring context (integration tests)

## Test Types

### Unit Tests

**Scope:**
- Pure utility classes in `common` module
- Password validation rules
- Expression engine evaluation
- Any standalone business logic

**Approach:**
- Direct instantiation testing
- Parameterized tests for multiple input scenarios

**Example Locations:**
- `common/src/test/java/cn/opensrcdevelop/common/srcurity/password/rule/`
- `common/src/test/java/cn/opensrcdevelop/common/expression/`

### Integration Tests

**Scope:**
- Client SDK tests in `auth-client-spring-boot-starter`
- AI tool tests in `ai-chatbi`

**Example:**
```java
// auth-client-spring-boot-starter/src/test/java/
// Uses Spring context with test configuration
@Configuration
public class ServerSecurityConfig {
    @Bean
    public SecurityFilterChain clientServerSecurityFilterChain(HttpSecurity http) throws Exception {
        http
            .authorizeHttpRequests(authorize -> authorize.anyRequest().authenticated())
            .oauth2Login(Customizer.withDefaults());
        return http.build();
    }
}
```

### E2E Tests

**Framework:** Playwright

**Location:** `ui/e2e/` (referenced in package.json)

**Run:**
```bash
npm run test:e2e
```

**Note:** E2E tests require setup:
```bash
npm run test:e2e:install   # Install Playwright browsers
```

## Common Patterns

### Async Testing

**Not observed in existing tests.** For testing async code:
- Use `@Async` test utilities
- Consider `CompletableFuture` testing patterns

### Error Testing

**Pattern from codebase:**
```java
@Test
void evaluateTest2() {
    String expression = "System.exit(0)";
    assertNull(expressionEngine.evaluate(expression));
}
```

### Parameterized Testing

**Pattern:**
```java
@ParameterizedTest
@MethodSource("provideTestCases")
void testValidate(String password, boolean expected) {
    assertEquals(expected, rule.validate(password));
}

private static Stream<Arguments> provideTestCases() {
    return Stream.of(
        Arguments.of("input1", "expected1"),
        Arguments.of("input2", "expected2")
    );
}
```

## Frontend Testing

### E2E Tests

**Framework:** Playwright

**Configuration:**
- No `playwright.config.ts` in project root
- Uses npm scripts from `ui/package.json`

**Test Files:**
- Location: `ui/e2e/` (not explored in detail)

**Fixtures:**
- Custom fixtures via `ui/e2e/fixtures/`
- Setup: `npm run test:fixtures`

### Unit Tests

**Status:** Not configured

**Note:** No Jest, Vitest, or other unit test frameworks configured for frontend

## Testing Best Practices

### From Quality Gate Rules

1. **Write tests alongside code** - Not separately after business code
2. **Test data content, not just counts**
   - ❌ `assertEquals(1, userRepository.findAll().size())`
   - ✅ Verify actual field values: `assertEquals("expectedUsername", savedUser.getUsername())`
3. **Test boundary conditions** - Null, empty, max/min values
4. **Test error scenarios** - Verify exception handling
5. **Use meaningful assertion messages**

### Code Coverage Priorities

Based on existing test files:
1. **Password validation** (`common` module) - High coverage
2. **Expression engine** (`common` module) - Basic coverage
3. **Client SDK** (`auth-client-spring-boot-starter`) - Integration tests
4. **AI tools** (`ai-chatbi`) - Tool execution tests

### Missing Test Coverage

- **auth-biz services** - No unit tests found for service layer
- **Controllers** - No HTTP-level tests
- **Repositories** - No DAO-level tests
- **Security components** - No security flow tests

---

*Testing analysis: 2026-03-19*
