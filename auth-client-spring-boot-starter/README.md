# 鉴权客户端（Auth Client）

基于 [**Spring Security OAuth2 Client**](https://docs.spring.io/spring-security/reference/reactive/oauth2/client/index.html) 为业务客户端提供鉴权服务。向 **Auth Server** 发起请求，获取登录用户在当前业务客户端下的权限信息，支持扩展鉴权上下文，解析授权限制条件，提供细粒度的权限控制。

### 如何使用

- 添加依赖

  ```groovy
  implementation 'cn.opensrcdevelop:auth-client-spring-boot-starter:latest'
  ```

- 向 `application.properties` 中添加配置项

  ```properties
  # Auth Server 地址
  auth.client.issuer
  # 权限信息
  # permissionName 替换为自定义权限名称，ressource 为当前权限的资源标识，permission 为当前权限的权限标识
  auth.client.authorize.permissionName.resource
  auth.client.authorize.permissionName.permission
  ```

- 向需要鉴权的 API 接口添加注解 **@Authorize** 执行鉴权

  String[] value：所需的权限名称集合

  boolean anyMatch：任意匹配，默认：true

  ```java
  // 登录用户拥有任意权限，鉴权通过
  @Authorize({ "permissionName", "otherPermissionName" })
  
  // 登录用户拥有全部权限，鉴权通过
  @Authorize(value = { "permissionName", "otherPermissionName" }, anyMatch = false)
  ```

### 如何扩展鉴权上下文

- 向 Spring 容器中添加一个或多个 **OAuth2AttributesCustomizer** 对象。初始上下文将包含 Auth Server 发行的 **id_token** 中包含的全部 **Claim** 及拦截的方法调用（**MethodInvocation**），可以添加或删除属性，扩展鉴权上下文

  ```java
  @Bean
  public OAuth2AttributesCustomizer oAuth2AttributesCustomizer() {
      return oAuth2Attributes -> {
          // 添加属性
          oAuth2Attributes.setAttribute("ip", ip);
          
          // 删除属性
          oAuth2Attributes.removeAttribute("password");
      };
  }
  ```

- 在 Auth Server 限制条件的 **SpringEL 表达式**中使用鉴权上下文中属性

  SpringEL 表达式示例：

  - 限制访问 ip

    `ip == '192.168.10.18'`

  - 限制访问时间

    `T(java.time.LocalTime).now() >= T(java.time.LocalTime).of(9, 0) && T(java.time.LocalTime).now() <= T(java.time.LocalTime).of(18, 0)`
  
- 在 SpringEL 表达式中调用自定义方法校验鉴权上下文

  - 实现接口：`ICheckAttribute` ，向 Spring 中注入 Bean

    ```java
    @Component("customCheckAttribute")
    public class CustomCheckAttribute implements ICheckAttribute {
    
        @Override
        public boolean check(Map<String, Object> attributes) {
            return false;
        }
    }
    ```

  - 获取拦截的方法调用

    `MethodInvocation methodInvocation = (MethodInvocation) attributes.get(AuthClientConstants.METHOD_INVOCATION);`

  - 在 SpringEL 表达式中调用方法

    `@customCheckAttribute.check(#root)`

    
