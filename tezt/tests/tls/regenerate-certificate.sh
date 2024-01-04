#!/bin/sh

# This command regenerates the files tezos.key and tezos.crt, used in
# the Tezt test 'Test TLS'.

openssl req \
  -x509 \
  -newkey rsa:2048 \
  -sha256 \
  -days 36500 \
  -nodes \
  -keyout tezt/tests/tls/tezos.key \
  -out tezt/tests/tls/tezos.crt \
  -subj "/CN=Easy-RSA CA" \
  -addext 'basicConstraints = CA:false' \
  -addext "subjectAltName = DNS:localhost" \
  -text
