FROM haskell:8.8.3

WORKDIR /app
ADD . /app

RUN apt-get update && \
  apt-get install -y libpq-dev && \
  stack build

EXPOSE 3000

CMD ["stack", "run"]
