#! /usr/bin/env bash

docker run \
  -v `pwd`:/home/gusdev/go \
  -v `pwd`:/home/gusdev/rescoyl \
  -t images.reesd.com/reesd/stack \
  cabal install rescoyl/rescoyl.cabal
