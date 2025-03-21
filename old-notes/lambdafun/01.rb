Identity = ->x { x }

True = ->first{->second{first}}
False = ->first{->second{second}}

If = ->bool{->trueCase{->falseCase{
  bool[trueCase][falseCase].(Identity)
}}}

Assert = ->bool {
  If[bool][->_{ puts "✔" }][->_{ puts "✘" }]
}

Assert[True]
Assert[False]

#
# $ ruby -w ./01.rb 
# ✔
# ✘
#
