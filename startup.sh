#!/bin/sh

export SBCL_HOME=~/sbcl/contrib/

while ! sh ~/sbcl/run-sbcl.sh --dynamic-space-size 1024  \
    --lose-on-corruption \
    --disable-ldb \
    --no-sysinit \
    --userinit ~/ql-sbclrc \
    --disable-debugger \
    --load "start-bots.lisp"
    do
    sleep 5
done
