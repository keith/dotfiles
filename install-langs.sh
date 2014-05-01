#!/usr/bin/env bash

cd langs
langs=(*.sh)

for lang in ${langs[@]}
do
    ./$lang install
done
