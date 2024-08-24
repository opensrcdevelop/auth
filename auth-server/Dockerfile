# Docker image for springboot file run
# Docker Docs: https://docs.docker.com/

FROM amazoncorretto:17.0.11-alpine3.16
COPY ./build/libs/auth-server-*.jar /app/app.jar
ENV LANG C.UTF-8
ENV TZ=Asia/Shanghai
RUN ln -snf /usr/share/zoneinfo/'$TZ' /etc/localtime && echo '$TZ' > /etc/timezone
ENTRYPOINT ["java", "-Dspring.profiles.active=prod", "-jar"]
CMD ["/app/app.jar"]