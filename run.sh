#!/bin/bash

VERSION=20160503-2
IMAGE=dragonflyscience/dragonfly-website
USERIMAGE=$USER-dragonfly-website:$VERSION
INTERACTIVE=$([ -t 0 ] && echo '-it')
PORT=${PORT:=8000}

docker inspect $IMAGE >/dev/null 2>&1
if [ $? != 0 ]; then
    docker build -t "$IMAGE" .
fi

docker inspect $USERIMAGE >/dev/null 2>&1
if [ $? != 0 ]; then
    cat dockertemplate | USERID=$(id -u) GROUPID=$(id -g) envsubst | docker build -t "$USERIMAGE" -
fi

mkdir -p $HOME/.stack &&

docker run --rm $INTERACTIVE -p $PORT:8000 -u $(id -u):$(id -g) \
  -w /work -v $PWD:/work \
  -v $HOME/.stack:/$HOME/.stack \
  -e STACK_ROOT=/$HOME/.stack \
  $USERIMAGE ./build.sh $*

