# BUILD
FROM haskell:8.8.3 AS builder
WORKDIR /build
ADD . /build
RUN apt-get update && \
  apt-get install -y libpq-dev && \
  stack build
RUN cp $(stack exec which pizza-delivery-exe) ./

# RUN
FROM ubuntu:20.04
WORKDIR /app
COPY --from=builder /build/pizza-delivery-exe .
RUN apt-get update && \
  apt-get install -y libpq-dev
EXPOSE 3000
CMD ["./pizza-delivery-exe"]