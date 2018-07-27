x, y = 5, 6

# TODO: this is not working.

puts case
  when x > y
    return x + y
    exit
  when x < y
    y - x
    exit
  when x == y
    x
  else
    0
  end

#
# If nothing matches, not even the else (which is not the case for this
# example), then `nil' is returned.
#
