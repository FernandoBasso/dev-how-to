
def crappy()
  yield(1, 2, 3, 4, 5)
end

crappy do |foo, bar = 0, *rest, x, y|
  p foo, bar, rest, x, y
end


#
# We define the sponge params with `*rest', and use it as `rest' (without the
# *) inside the block. `rest' then is an _array_ containing zero or more
# sponged up arguments.
#
# BEWARE: If we use `*rest' (with the *) _inside_ the block, then it behaves
# like a normal argument name, and does not become an array. It just stands
# for one normal, and required argument.
#



