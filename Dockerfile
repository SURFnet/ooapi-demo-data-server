FROM clojure:temurin-21-tools-deps-noble as builder
RUN mkdir /app
WORKDIR /app
COPY ./deps.edn /app/deps.edn
RUN clojure -P -A:build
COPY . /app/
RUN clojure -T:build uberjar

FROM gcr.io/distroless/java21-debian12
COPY --from=builder /app/target/ooapi-demo-data-server.jar /ooapi-demo-data-server.jar
COPY HealthCheck.java .
EXPOSE 8080
CMD ["ooapi-demo-data-server.jar"]
HEALTHCHECK --interval=30s --timeout=3s --start-period=15s --retries=2 CMD ["java", "HealthCheck.java", "||", "exit", "1"]
