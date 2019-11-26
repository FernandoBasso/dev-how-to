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

[[ "$x" -lt "$y" ]] && printf '%s\n' 'X is less than Y'
[[ "$x" -gt "$y" ]] && printf '%s\n' 'X is greater than Y'
[[ "$x" -eq "$y" ]] && printf '%s\n' 'X is equal to Y'

#
# This solution uses the old, sh-compatible `test` command. It also shows that
# [[ ]] can use -gt style of operators as well.
#
# Read
#
#     help [
#     help test
#
# And/or this section of the Bash manual:
#
# https://www.gnu.org/software/bash/manual/bash.html#Bash-Conditional-Expressions
#
# https://mywiki.wooledge.org/BashFAQ/031
#

