#!/bin/bash

#
# Comparing Numbers
# =================
#
# https://www.hackerrank.com/challenges/bash-tutorials---comparing-numbers
#
# Running
# -------
#
#     bash script.sh <<<$'3\n5'
#     printf '%d\n' 3 5 | bash script.sh
#

read -r x <&0
read -r y <&0

if [[ "$x" < "$y" ]]
then
  printf '%s\n' 'X is less than Y'
elif [[ "$x" > "$y" ]]
then
  printf '%s\n' 'X is greater than Y'
else
  printf '%s\n' 'X is equal to Y'
fi

#
# This solution uses the newer [[ test command.
#
# Read
#
#     help [[
#
# And/or this section of the Bash manual:
#
# https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs
#
# https://mywiki.wooledge.org/BashFAQ/031
#

