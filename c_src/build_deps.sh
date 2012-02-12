#!/bin/bash

TINYSTM_VSN="1.0.3"

set -e

if [ `basename $PWD` != "c_src" ]; then
    pushd c_src
fi

BASEDIR="$PWD"

case "$1" in
    clean)
        rm -rf tinySTM-$TINYSTM_VSN
        ;;

    test)
        (cd tinySTM-$TINYSTM_VSN && make check)

        ;;
    *)
        export NOTLS=1

        if [ ! -d tinySTM-$TINYSTM_VSN ]; then
            tar -xzf tinySTM-$TINYSTM_VSN.tgz
        fi

        (cd tinySTM-$TINYSTM_VSN && make all)

        ;;
esac

