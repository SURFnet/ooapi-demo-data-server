FROM clojure:temurin-17-tools-deps-focal as builder
RUN mkdir /app
WORKDIR /app
COPY ./deps.edn /app/deps.edn
RUN clojure -P -Abuild
COPY . /app/
RUN clojure -T:build uberjar

FROM gcr.io/distroless/java17-debian11
COPY --from=builder /app/target/ooapi-demo-data-server.jar /ooapi-demo-data-server.jar
COPY HealthCheck.java .
EXPOSE 8080
CMD ["ooapi-demo-data-server.jar"]
HEALTHCHECK --interval=30s --timeout=3s --start-period=15s --retries=2 CMD ["java", "HealthCheck.java", "||", "exit", "1"]
