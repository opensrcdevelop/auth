# Docker image for springboot file run
# Docker Docs: https://docs.docker.com/

FROM amazoncorretto:21-alpine3.20

ARG VERSION

RUN addgroup -g 1000 appgroup && adduser -u 1000 -G appgroup -s /bin/sh -D appuser

COPY ./build/libs/auth-server-*.jar /app/app.jar
COPY ./elastic-apm/elastic-apm-agent-*.jar /app/elastic-apm/elastic-apm-agent.jar
RUN chown -R appuser:appgroup /app

ENV LANG C.UTF-8
ENV TZ=Asia/Shanghai
ENV APP_VERSION=${VERSION}
RUN ln -snf /usr/share/zoneinfo/'${TZ}' /etc/localtime && echo '${TZ}' > /etc/timezone
RUN apk add --no-cache fontconfig ttf-dejavu python3 py3-pip

USER appuser
WORKDIR /app
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -Dspring.profiles.active=prod -jar /app/app.jar"]