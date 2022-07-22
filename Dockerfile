FROM clojure:tools-deps as builder
RUN mkdir /app
WORKDIR /app
COPY . /app/
RUN clojure -X:depstar uberjar :jar ooapi-demo-data-server.jar

FROM openjdk:14-alpine
COPY --from=builder /app/ooapi-demo-data-server.jar /ooapi-demo-data-server.jar
ENTRYPOINT ["java", "-cp", "ooapi-demo-data-server.jar", "clojure.main", "-m", "ooapi-demo-data-server.core"]
