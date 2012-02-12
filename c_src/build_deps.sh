#!/bin/bash

TINYSTM_VSN="1.0.3"

set -e

if [ `basename $PWD` != "c_src" ]; then
    pushd c_src
fi

BASEDIR="$PWD"

case "$1" in
    clean)
        rm -rf tinySTM
        ;;

    test)
        (cd tinySTM && make check)

        ;;
    *)
        export NOTLS=1

        if [ ! -d tinySTM ]; then
            tar -xzf tinySTM-$TINYSTM_VSN.tgz
            mv tinySTM-$TINYSTM_VSN tinySTM
        fi

        (cd tinySTM && make all)

        ;;
esac

