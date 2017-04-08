#! /usr/bin/env bash

STACK_IMAGE=${1:-7.8.4}

docker run \
  -v `pwd`:/home/gusdev/go \
  -v `pwd`:/home/gusdev/rescoyl \
  -t images.reesd.com/reesd/stack:$STACK_IMAGE \
  cabal install rescoyl/rescoyl.cabal
