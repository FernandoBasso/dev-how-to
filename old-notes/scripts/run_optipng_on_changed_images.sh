#!/usr/bin/env bash

##
# This script uses some git status output to collect an array of
# ‘.png’ files and then runs ‘optipng’ on each of them.
##

imgs=(\
	$(git status . --short --porcelain \
	| grep '\.png$' \
	| cut -d ' ' -f 2 \
	) \
)

for img in "${imgs[@]}"
do
	optipng -o7 "$img"
done

