#!/bin/bash

read -r x <&0
read -r y <&0

# Array of messages.
msgs=('X == ' 'X < Y' 'X > Y')

# Array of 0s and 1s as per bc's output for each operation
mapfile -t bools < <(printf '%s\n' "$x"{==,\<,\>}"$y" | bc)

# Calculate the index of the message.
idx=$(( "${bools[0]}" * 0 + "${bools[1]}" * 1 + "${bools[2]}" * 2 ))

# Prints the message.
printf '%s\n' "${msgs[$idx]}"

