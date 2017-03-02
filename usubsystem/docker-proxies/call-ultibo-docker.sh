#!/bin/bash

uname -m | grep -iq '^arm.*'
if [[ $? == 0 ]]
then
    VERSION=arm7hf@sha256:aaf9751128ff928adb35e97a4addfb2bfffe156719474b10b183d72162ce862d
else
    VERSION=x64@sha256:66586c5b04fca9e967157abfd1b3bbb96de3ff47757ccddd1e086e0e36f3605c
fi

DOCKER_IMAGE=markfirmware/ultibo-docker2-$VERSION

#echo using docker image $DOCKER_IMAGE
#echo "    for: $*"
docker run --rm -i -v $(pwd):/workdir --entrypoint /bin/bash $DOCKER_IMAGE -c "$*"
