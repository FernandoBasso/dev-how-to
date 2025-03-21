Identity = ->x { x }

True = ->first{->second{first}}
False = ->first{->second{second}}

If = ->bool{->trueCase{->falseCase{
  bool[trueCase][falseCase].(Identity)
}}}

Assert = ->bool {
  If[bool][->_{ puts "✔" }][->_{ puts "✘" }]
}

Refute = ->bool {
  If[bool][->_{ puts "✘" }][->_{ puts "✔" }]
}

$zero =Identity
IsZero = ->_{ True }

Succ = ->_{ $zero }

NumbersEqual = ->first{->second{
  True
}}

Assert[IsZero[$zero]]
Assert[NumbersEqual[$zero][$zero]]
Assert[NumbersEqual[Succ[$zero]][Succ[$zero]]]

Refute[IsZero[Succ[$zero]]]
Refute[NumbersEqual[Succ[$zero]][$zero]]

#
# $ ruby -w ./02.rb
# ✔
# ✔
# ✔
# ✘
# ✘
#
# The refutations above should also return ✔, but they
# don't 😭.
#
