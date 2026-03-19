# Docker image for springboot file run
# Docker Docs: https://docs.docker.com/

FROM eclipse-temurin:21-jre-jammy

ARG VERSION

RUN groupadd -g 1000 appgroup && \
    useradd -u 1000 -g appgroup -s /bin/bash -m appuser

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        fontconfig \
        fonts-dejavu \
        python3 \
        python3-pip \
        python3-venv && \
    ln -s /usr/bin/python3 /usr/bin/python && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

COPY ./build/libs/auth-server-*.jar /app/app.jar
COPY ./elastic-apm/elastic-apm-agent-*.jar /app/elastic-apm/elastic-apm-agent.jar

RUN chown -R appuser:appgroup /app

ENV LANG C.UTF-8
ENV TZ=Asia/Shanghai
ENV APP_VERSION=${VERSION}

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

USER appuser
WORKDIR /app

ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -Dspring.profiles.active=prod -jar /app/app.jar"]