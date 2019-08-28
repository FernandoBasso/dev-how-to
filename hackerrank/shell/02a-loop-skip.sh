#!/usr/bin/env bash

for num in {1..99} ; do # <1>
    if (( $num % 2 )) ; then # <2>
        printf '%d\n' $num
    fi
done

#
# 1.
# echo $(( 4 % 2 )) produces 0.
#
# echo $(( 3 % 2 )) produces 1.
#
# 0 is truthy, 1 is falsy.
#
# https://unix.stackexchange.com/questions/110348/how-do-i-get-the-list-of-exit-codes-and-or-return-codes-and-meaning-for-a-comm
#
# In C/bash and other langs, 0 is truthy because all other positive or negative
# integer can then be used to represent a type of error, something similar to
# HTTP status codes. Try `man ping` and search for `code 1` and `code 2` or
# `man wget` and search for `EXIT STATUS`.
#

#
# 2. Could also use `$(seq 99)` instead of the range. It would be using another
#    process and an external program, but it would work.
#
# Also,
#
#   `seq 1 2 99`
#
# would start at 1, increment by 2, and stop on 99. With a range:
#
#   {1..99..2}`
#
# would do the same.
#
