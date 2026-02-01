# Docker image for springboot file run
# Docker Docs: https://docs.docker.com/

# Build arguments (作用域: 用于 Dockerfile 其余部分)
ARG VERSION=dev-SNAPSHOT

FROM amazoncorretto:21-alpine3.20

# 重新声明 ARG 以便在当前构建阶段使用
ARG VERSION

COPY ./build/libs/auth-server-*.jar /app/app.jar
COPY ./elastic-apm/elastic-apm-agent-*.jar /app/elastic-apm/elastic-apm-agent.jar
ENV LANG C.UTF-8
ENV TZ=Asia/Shanghai
ENV APP_VERSION=${VERSION}
RUN ln -snf /usr/share/zoneinfo/'${TZ}' /etc/localtime && echo '${TZ}' > /etc/timezone
RUN apk --no-cache add fontconfig ttf-dejavu
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -Dapp.version=${APP_VERSION} -Dspring.profiles.active=prod -jar /app/app.jar"]