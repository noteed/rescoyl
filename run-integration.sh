#! /bin/bash

# This script can be called like so:
#
#    > ./run-integration.sh yes ip:port
#    > ./run-integration.sh yes ip
#    > ./run-integration.sh no ip:port
#    > ./run-integration.sh no ip
#
# "yes" means that Basic auth is done by Nginx, while "no" means Basic auth is
# taken care of by the upstream server (i.e. the Docker registry itself).
#
# The ip and optional port are those of the registry to test.
#
# After the run, a mitmproxy trace is available as either `mitm-output.txt`.

NGINX_AUTH=$1
TEST_IP_PORT=$2

# The docker client wants HTTPS with valid certificate. We generate a
# self-signed certificate.
./generate-certificate.sh

# We generate a password files (used by Nginx if it is in charge of Basic
# auth). In the case of Rescoyl, an equivalent file called `sample.users`
# is injected in the Docker image.
echo quux:$(openssl passwd -crypt thud) > nginx/registry.passwd

# Select the Nginx configuration file with or without Basic auth.
if [ $NGINX_AUTH == "yes" ] ; then
  NGINX_CONF=registry.local
  sed -i "s/^  server  .*/  server  $TEST_IP_PORT;/" nginx/registry.local
else
  NGINX_CONF=rescoyl.local
  sed -i "s/^  server  .*/  server  $TEST_IP_PORT;/" nginx/rescoyl.local
fi

# Run Nginx, passing the chosen configuration file, password file,
# self-signed certificate, and key.
NGINX_ID=$(docker run -d \
  -v `pwd`/static:/usr/share/nginx/www \
  -v `pwd`/nginx/$NGINX_CONF:/etc/nginx/sites-enabled/registry.local \
  -v `pwd`/nginx/registry.passwd:/etc/nginx/registry.passwd \
  -v `pwd`/self-private.key:/etc/nginx/self-private.key \
  -v `pwd`/self-certificate.crt:/etc/nginx/self-certificate.crt \
  noteed/nginx
  )
NGINX_IP=$(docker inspect $NGINX_ID | grep IPAddress | awk '{ print $2 }' | tr -d ',"')

# Run mitmproxy. mitmproxy traces can be inspected with:
#
#   > docker run -e LANG=en_US.UTF-8 -v `pwd`:/source -t -i mitmproxy \
#     mitmproxy -r /source/mitm-output.txt
rm -f mitm-output.txt
touch mitm-output.txt
MITM_ID=$(docker run -d \
  -v `pwd`/mitm-output.txt:/mitm-output.txt \
  -v `pwd`/self-key-and-certificate.pem:/self-key-and-certificate.pem \
  mitmproxy \
    mitmdump -q -P https://$NGINX_IP -p 443 -w /mitm-output.txt \
    --cert /self-key-and-certificate.pem
  )
REGISTRY_IP=$(docker inspect $MITM_ID | grep IPAddress | awk '{ print $2 }' | tr -d ',"')

# Setup a static DNS server so that the test container sees the tested
# container as registry.local.
mkdir -p dnsmasq.d
echo "address=/registry.local/$REGISTRY_IP" > dnsmasq.d/0hosts
DNS=$(
  docker run \
  --privileged \
  -d -p 53 \
  -v `pwd`/dnsmasq.d:/etc/dnsmasq.d/ \
  dnsmasq)
DNS_IP=$(docker inspect $DNS | grep IPAddress | awk '{ print $2 }' | tr -d ',"')

sleep 5

# If the upstream server displays a nice line, well, we display a nice line.
# TODO This should return an application/json string.
curl --cacert self-certificate.crt \
  --resolve "registry.local:443:$REGISTRY_IP" \
  https://registry.local
echo

# Run our test script within its own container. This is a docker-in-docker
# container. (A slight variant of https://github.com/jpetazzo/dind with
# specific versions of docker.)
# The --volume-from is there to reuse already `docker pull`ed ubuntu:12.04
# layers.
docker run --privileged --dns $DNS_IP -t -i \
  -v `pwd`:/source \
  --volumes-from dind-rescoyl \
  noteed/dind:1.1.2 wrapdocker /source/test.sh

# Some cleanup.
docker kill $DNS $NGINX_ID $MITM_ID
docker rm $DNS $NGINX_ID $MITM_ID
rm -f self-certificate.crt self-key-and-certificate.pem self-private.key
rm -f nginx/registry.passwd
