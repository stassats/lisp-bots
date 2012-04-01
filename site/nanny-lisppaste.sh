#!/bin/sh
ulimit -t 86400
ulimit -l 1572864 # 86400s and 1572864kB
while : ; do
   echo 'starting'
   SBCL_HOME=/home/lisppaste/sbcl_1048_test/lib/sbcl/ \
     /home/lisppaste/sbcl_1048_test/bin/sbcl --dynamic-space-size 1024 \
                                             --eval '(load "start-critters")'
#   sbcl --eval '(load "start-critters")'
done
