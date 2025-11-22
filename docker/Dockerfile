FROM hseeberger/scala-sbt:eclipse-temurin-11.0.14.1_1.6.2_2.13.8 as build

WORKDIR /app

COPY . .

RUN sbt update compile stage

FROM eclipse-temurin:21-alpine

WORKDIR /app
RUN apk add --no-cache bash

COPY --from=build /app/target/universal/stage /app

COPY docker/entrypoint.sh /app/entrypoint.sh

RUN chmod +x /app/entrypoint.sh && sed -i 's/\r$//' /app/entrypoint.sh

ENTRYPOINT ["/bin/bash", "/app/entrypoint.sh"]
