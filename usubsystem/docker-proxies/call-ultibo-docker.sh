#!/bin/bash

DOCKER_IMAGE=markfirmware/ultibo-docker-x64-v1  # x64  armv7ahf
#echo using docker image $DOCKER_IMAGE
#echo "    for: $*"
docker run --rm -i -v $(pwd):/workdir --entrypoint /bin/bash $DOCKER_IMAGE -c "$*"
