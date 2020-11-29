FROM clojure:tools-deps as builder
RUN mkdir /app
WORKDIR /app
COPY . /app/
RUN clojure -X:depstar uberjar :jar instant-ooapi.jar

FROM openjdk:14-alpine
COPY --from=builder /app/instant-ooapi.jar /instant-ooapi.jar
ENTRYPOINT ["java", "-cp", "instant-ooapi.jar", "clojure.main", "-m", "instant-ooapi.core"]
