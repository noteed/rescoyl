#! /bin/bash

# Generate self-private.key, and self-certificate.crt for Nginx, and
# self-key-and-certificate.pem, for mitmproxy.
# See in test.sh how the certificate is made "trusted" by the docker client.

answers() {
  echo --
  echo Test State
  echo Test City
  echo Test Organization
  echo Test Organizational Unit
  echo registry.local
  echo root@registry.local
}

answers | /usr/bin/openssl req \
  -newkey rsa:2048 \
  -keyout self-private.key \
  -out self-certificate.crt \
  -nodes -x509 -days 365 \
  2> /dev/null

cat self-private.key self-certificate.crt > self-key-and-certificate.pem
