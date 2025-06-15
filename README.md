# 认证授权服务（Auth Server）

认证授权服务（Auth Server）以 [**Spring Authorization Server**](https://spring.io/projects/spring-authorization-server) 为框架，提供身份认证协议 **OAuth 2.0** 和 **OIDC** 的实现。扩展其功能，并基于 Spring Security 提供授权鉴权服务。

### 主要技术

Java 21、Gradle、PostgreSQL、Redis、Spring Boot 3、Spring Authorization Server、Spring Security、Spring Session、Spring Doc、MyBatis、MyBatis-Plus、Flyway

### 主要功能

客户端、用户管理、资源管理、权限管理、多租户、个人中心、数据字典、身份源管理、系统设置

### Guide

[Auth Server Guide](https://zippy-fireplace-aab.notion.site/Auth-Server-Guide-2131bf1df6e180b49026e77aade2878c)

### 快速开始

#### 使用 Docker

- 执行以下命令，下载  `docker-compose.yml` 文件

  ```bash
  wget https://githubraw.com/opensrcdevelop/auth/main/deploy/docker/docker-compose.yml -O docker-compose.yaml
  ```

- 执行以下 `docker-compose`  命令

  ```bash
  docker-compose -f docker-compose.yaml up
  ```
  
- 访问地址：http://localhost:6543

- 初始账号 / 密码：admin / 123456
