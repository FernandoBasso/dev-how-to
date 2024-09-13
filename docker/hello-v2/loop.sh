#!/usr/bin/env bash

##
# Loops from 1 to 100 with a two-second interval between
# each int and, printing each of them in turn.
#
for i in {1..300}
do
  printf '%d\n' $i
  sleep 2
done
