#!/bin/sh
ulimit -t 86400
ulimit -l 1572864 # 86400s and 1572864kB

export SBCL_HOME=~/sbcl-new/contrib/

while ! sh ~/sbcl-new/run-sbcl.sh --dynamic-space-size 1024  \
    --lose-on-corruption \
    --disable-ldb \
    --userinit ~/ql-sbclrc \
    --disable-debugger \
    --load "start-paste.lisp"
    do
    sleep 5
done
