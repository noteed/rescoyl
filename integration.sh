#! /bin/bash

# This can run against the official docker-registry project, or against
# rescoyl. Simply run
#
#    > ./integration.sh registry
#
# or
#
#    > ./integration.sh rescoyl


# The docker client wants HTTPS with valid certificate. We generate a
# self-signed certificate.
./generate-certificate.sh


# Run either the official registry v1, registry v2, or Rescoyl.
case $1 in
"registry-v1")
  REGISTRY_ID=$(docker run -d registry)
  export NGINX_CONF=registry.local
  ;;
"registry-v2")
  export REGISTRY_ID=$(docker run -d \
    -v `pwd`/self-private.key:/self-private.key \
    -v `pwd`/self-certificate.crt:/self-certificate.crt \
    -e REGISTRY_HTTP_TLS_CERTIFICATE=/self-certificate.crt \
    -e REGISTRY_HTTP_TLS_KEY=/self-private.key \
    -e REGISTRY_HTTP_ADDR=0.0.0.0:443 \
    -e REGISTRY_LOG_LEVEL=debug \
    registry:2
    )
  export NGINX_CONF=registry2.local
  ;;
"rescoyl")
  REGISTRY_ID=$(docker run -d noteed/rescoyl)
  export NGINX_CONF=rescoyl.local
  ;;
*)
  echo "Usage: either registry-v1, registry-v2, or rescoyl."
  exit 1;
  ;;
esac

REGISTRY_IP=$(docker inspect $REGISTRY_ID | grep IPAddress | awk '{ print $2 }' | tr -d ',"')

# Run some tests againt it.
if [ $1 == "rescoyl" ] ; then
  ./run-integration.sh no $REGISTRY_IP
  mv mitm-output.txt mitm-${1}.txt

  sleep 5
  sudo docker cp $REGISTRY_ID:/log .
  sudo docker cp $REGISTRY_ID:/store .
else
  ./run-integration.sh yes $REGISTRY_IP:5000
  mv mitm-output.txt mitm-${1}.txt
fi

docker kill $REGISTRY_ID
docker rm $REGISTRY_ID
