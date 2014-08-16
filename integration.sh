#! /bin/bash

# This can run against the official docker-registry project, or against
# rescoyl. Simply run
#
#    > ./integration.sh registry
#
# or
#
#    > ./integration.sh rescoyl

# Run either the official registry or Rescoyl.
if [ $1 == "registry" ] ; then
  REGISTRY_ID=$(docker run -d registry)
  NGINX_CONF=registry.local
else
  REGISTRY_ID=$(docker run -d noteed/rescoyl)
  NGINX_CONF=rescoyl.local
fi

REGISTRY_IP=$(docker inspect $REGISTRY_ID | grep IPAddress | awk '{ print $2 }' | tr -d ',"')

# Run some tests againt it.
if [ $1 == "registry" ] ; then
  ./run-integration.sh yes $REGISTRY_IP:5000
  mv mitm-output.txt mitm-registry.txt
else
  ./run-integration.sh no $REGISTRY_IP
  mv mitm-output.txt mitm-rescoyl.txt
fi

docker kill $REGISTRY_ID
docker rm $REGISTRY_ID
