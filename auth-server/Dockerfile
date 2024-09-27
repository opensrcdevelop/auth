# Docker image for springboot file run
# Docker Docs: https://docs.docker.com/

FROM amazoncorretto:17.0.11-alpine3.16
COPY ./build/libs/auth-server-*.jar /app/app.jar
COPY ./elastic-apm/elastic-apm-agent-*.jar /app/elastic-apm/elastic-apm-agent.jar
ENV LANG C.UTF-8
ENV TZ=Asia/Shanghai
RUN ln -snf /usr/share/zoneinfo/'$TZ' /etc/localtime && echo '$TZ' > /etc/timezone
ENV JAVA_OPTS=""
ENTRYPOINT ["java","$JAVA_OPTS", "-javaagent:/app/elastic-apm/elastic-apm-agent.jar -Delastic.apm.service_name=auth-server -Delastic.apm.server_url=http://apm-server.ns-bb45gt31.svc.cluster.local:8200 -Delastic.apm.application_packages=cn.opensrcdevelop -Delastic.apm.environment=prod -Delastic.apm.enable_log_correlation=true -Dspring.profiles.active=prod", "-jar"]
CMD ["/app/app.jar"]