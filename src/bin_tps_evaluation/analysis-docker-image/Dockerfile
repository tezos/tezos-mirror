FROM postgres:14-alpine

ENV POSTGRES_HOST_AUTH_METHOD=trust
COPY init-tezos-db.sql /docker-entrypoint-initdb.d/init-tezos-db.sql

ENTRYPOINT ["docker-entrypoint.sh", "postgres"]
