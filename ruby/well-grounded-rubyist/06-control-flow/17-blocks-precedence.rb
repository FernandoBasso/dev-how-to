#
# BEWARE of the precedence.
#

arr = 1, 3, 5

puts arr.map { |e| e * 2} # <1>
# →  2
# →  6
# →  10

puts arr.map do |e| e * 2 end # <2>
# → #<Enumerator:0x000055a9f3394138>


#
# <1> is the same as:
#
#   puts (arr.map { |e| e * 2 })
#
# <2> is the same as:
#
#   puts (arr.map) do |e| e * 2 end # <2>
#
# In the second case, the code block is interpreted as being part of the call
# to puts, not the call to map. And if you call puts with a block, it ignores
# the block. So the do/end version is really equivalent to
#
#   puts array.map
#
