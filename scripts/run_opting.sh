#!/usr/bin/env bash

imgs=(\
        $(git status --short --porcelain \
        | grep '\.png$' \
        | cut -d ' ' -f 2 \
        ) \
)

for img in "${imgs[@]}"
do
        optipng -o7 "$img"
done
