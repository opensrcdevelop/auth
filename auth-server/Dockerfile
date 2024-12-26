# Docker image for springboot file run
# Docker Docs: https://docs.docker.com/

FROM bellsoft/liberica-openjre-debian:21-cds
COPY ./build/libs/auth-server-*.jar /app/application.jar
COPY ./elastic-apm/elastic-apm-agent-*.jar /app/elastic-apm/elastic-apm-agent.jar
ENV LANG C.UTF-8
ENV TZ=Asia/Shanghai
RUN ln -snf /usr/share/zoneinfo/'$TZ' /etc/localtime && echo '$TZ' > /etc/timezone
RUN apk --no-cache add fontconfig ttf-dejavu
# Extract the jar file using an efficient layout
RUN java -Djarmode=tools -jar /app/application.jar extract --layers --destination extracted
WORKDIR /app/application
# Execute the CDS training run
RUN java -XX:ArchiveClassesAtExit=application.jsa -Dspring.context.exit=onRefresh -jar application.jar
# This layout is efficient to start up and CDS friendly
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -XX:SharedArchiveFile=application.jsa -Dspring.profiles.active=prod -jar application.jar"]