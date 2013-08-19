#!/usr/bin/env bash

if ! which npm &> /dev/null;then
    echo "You must install node before installing its packages"
    exit
fi

nodes=(bower coffee-script grunt-cli)

for n in ${nodes[@]}
do
    npm install -g $n
done

