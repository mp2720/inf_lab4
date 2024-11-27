#!/usr/bin/env bash

for i in {1..100} ; do
    dist-newstyle/build/x86_64-linux/ghc-9.4.8/inf-lab4-0.1.0.0/x/lab4/build/lab4/lab4 < sched.json > /dev/null
done

