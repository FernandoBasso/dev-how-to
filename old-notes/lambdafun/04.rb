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

$zero = ->_{True}

Succ = ->n{ ->_{False} }
IsZero = ->n{ n.(Identity) }

NumbersEqual = ->first{->second{
  If[IsZero[first]][
    ->_{IsZero[second]}
  ][
    ->_{
      If[IsZero[second]][
        ->_{False}
      ][
        ->_{True}
      ]
    }
  ]
}}

Assert[IsZero[$zero]]
Assert[NumbersEqual[$zero][$zero]]
Assert[NumbersEqual[Succ[$zero]][Succ[$zero]]]

Refute[IsZero[Succ[$zero]]]
Refute[NumbersEqual[Succ[$zero]][$zero]]
Refute[NumbersEqual[$zero][Succ[$zero]]]
