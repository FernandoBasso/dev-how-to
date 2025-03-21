//
// This is an implementation in Go from Bruno Pinheiro:
//
// • https://github.com/brunopinheiro/
//

package main

import "fmt"

type Fn = func(arg LambdaSpec) LambdaSpec

type LambdaSpec interface {
	apply(arg LambdaSpec) LambdaSpec
}

type LambdaImpl struct {
	lambda Fn
}

func (l *LambdaImpl) apply(arg LambdaSpec) LambdaSpec {
	return l.lambda(arg)
}

func Def(l Fn) LambdaSpec {
	return &LambdaImpl{lambda: l}
}

var PRINT = func(text string) LambdaSpec {
	return Def(func(_ LambdaSpec) LambdaSpec {
		fmt.Print(text)
		return I
	})
}

func assert(title string, subject LambdaSpec) {
	subject.
		apply(PRINT("√: ")).
		apply(PRINT("X: ")).
		apply(I)

	fmt.Println(title)
}

func refute(title string, subject LambdaSpec) {
	subject.
		apply(PRINT("X: ")).
		apply(PRINT("√: ")).
		apply(I)

	fmt.Println(title)
}

// -----------------------------------------------------
// LAMBDA CALCULUS

var I = Def(func(x LambdaSpec) LambdaSpec { return x })

var T = Def(func(x LambdaSpec) LambdaSpec {
	return Def(func(_ LambdaSpec) LambdaSpec {
		return x
	})
})
var F = Def(func(_ LambdaSpec) LambdaSpec {
	return Def(func(y LambdaSpec) LambdaSpec {
		return y
	})
})
var IF = Def(func(x LambdaSpec) LambdaSpec {
	return Def(func(y LambdaSpec) LambdaSpec {
		return Def(func(z LambdaSpec) LambdaSpec {
			return x.apply(y).apply(z)
		})
	})
})

var NOT = Def(func(x LambdaSpec) LambdaSpec {
	return IF.apply(x).apply(F).apply(T)
})
var AND = Def(func(x LambdaSpec) LambdaSpec {
	return Def(func(y LambdaSpec) LambdaSpec {
		return IF.apply(x).apply(y).apply(x)
	})
})
var OR = Def(func(x LambdaSpec) LambdaSpec {
	return Def(func(y LambdaSpec) LambdaSpec {
		return IF.apply(x).apply(x).apply(y)
	})
})
var XOR = Def(func(x LambdaSpec) LambdaSpec {
	return Def(func(y LambdaSpec) LambdaSpec {
		return IF.apply(x).apply(NOT.apply(y)).apply(y)
	})
})
var NOR = Def(func(x LambdaSpec) LambdaSpec {
	return Def(func(y LambdaSpec) LambdaSpec {
		return NOT.apply(OR.apply(x).apply(y))
	})
})
var BEQ = Def(func(x LambdaSpec) LambdaSpec {
	return Def(func(y LambdaSpec) LambdaSpec {
		return IF.apply(x).apply(y).apply(NOT.apply(y))
	})
})

// STRUCTS

var PAIR = Def(func(x LambdaSpec) LambdaSpec {
	return Def(func(y LambdaSpec) LambdaSpec {
		return Def(func(z LambdaSpec) LambdaSpec {
			return z.apply(x).apply(y)
		})
	})
})

var HEAD = Def(func(x LambdaSpec) LambdaSpec {
	return x.apply(T)
})
var TAIL = Def(func(x LambdaSpec) LambdaSpec {
	return x.apply(F)
})

// NATURAL NUMBERS

var ZERO = PAIR.apply(T).apply(I)

var IZ = Def(func(x LambdaSpec) LambdaSpec {
	return HEAD.apply(x)
})

var S = Def(func(x LambdaSpec) LambdaSpec {
	return PAIR.apply(F).apply(x)
})

var P = Def(func(x LambdaSpec) LambdaSpec {
	return TAIL.apply(x)
})

var EQ = I
var GRT = I
var ADD = I
var MUL = I
var SUB = I

var PREC = Def(func(x LambdaSpec) LambdaSpec {
	return Def(func(y LambdaSpec) LambdaSpec {
		return Def(func(f LambdaSpec) LambdaSpec {
			return (PAIR.apply(I).apply(f)).
				apply(OR.
					apply(IZ.apply(x)).
					apply(IZ.apply(y)),
				).
				apply(P.apply(x)).
				apply(P.apply(y))
		})
	})
})

var Number = func(n uint) LambdaSpec {
	number := ZERO
	for i := uint(0); i < n; i++ {
		number = S.apply(number)
	}
	return number
}

func testStructs() {
	assert("pair(x, y) => first element is x", (PAIR.apply(T).apply(F)).apply(T))
	refute("pair(x, y) => second element is y", (PAIR.apply(T).apply(F)).apply(F))

	assert("head(pair(x, y)) => x", HEAD.apply(PAIR.apply(T).apply(F)))
	refute("tail(pair(x, y)) => y", TAIL.apply(PAIR.apply(T).apply(F)))
}

func testNaturalNumbers() {
	EQ = Def(func(x LambdaSpec) LambdaSpec {
		return Def(func(y LambdaSpec) LambdaSpec {
			return IF.
				apply(IZ.apply(x)).
				apply(IZ.apply(y)).
				apply(IF.
					apply(IZ.apply(y)).
					apply(F).
					apply(PREC.apply(x).apply(y).apply(EQ)),
				)
		})
	})

	GRT = Def(func(x LambdaSpec) LambdaSpec {
		return Def(func(y LambdaSpec) LambdaSpec {
			return IF.
				apply(IZ.apply(x)).
				apply(F).
				apply(
					IF.
						apply(IZ.apply(y)).
						apply(T).
						apply(PREC.apply(x).apply(y).apply(GRT)),
				)
		})
	})

	ADD = Def(func(x LambdaSpec) LambdaSpec {
		return Def(func(y LambdaSpec) LambdaSpec {
			return IF.
				apply(IZ.apply(x)).
				apply(y).
				apply(
					(PAIR.apply(I).apply(ADD)).
						apply(IZ.apply(x)).
						apply(P.apply(x)).
						apply(S.apply(y)),
				)
		})
	})

	SUB = Def(func(x LambdaSpec) LambdaSpec {
		return Def(func(y LambdaSpec) LambdaSpec {
			return IF.
				apply(IZ.apply(x)).
				apply(ZERO).
				apply(
					IF.
						apply(IZ.apply(y)).
						apply(x).
						apply(PREC.apply(x).apply(y).apply(SUB)),
				)
		})
	})

	MUL = Def(func(x LambdaSpec) LambdaSpec {
		return Def(func(y LambdaSpec) LambdaSpec {
			return IF.
				apply(IZ.apply(x)).
				apply(ZERO).
				apply(
					IF.
						apply(IZ.apply(y)).
						apply(ZERO).
						apply(
							ADD.
								apply(y).
								apply(
									(PAIR.apply(I).apply(MUL)).
										apply(IZ.apply(x)).
										apply(P.apply(x)).
										apply(y),
								),
						),
				)
		})
	})

	assert("0 is 0", IZ.apply(ZERO))
	assert("0 == 0", EQ.apply(ZERO).apply(ZERO))
	refute("S(0) != 0", IZ.apply(S.apply(ZERO)))

	assert("S(0) == S(0)", EQ.
		apply(S.apply(ZERO)).
		apply(S.apply(ZERO)),
	)
	assert("S(S(0)) == S(S(0))", EQ.
		apply(S.apply(S.apply(ZERO))).
		apply(S.apply(S.apply(ZERO))),
	)

	assert("P(0) => 0", IZ.apply(P.apply(ZERO)))
	assert("P(S(0)) => 0", IZ.apply(P.apply(S.apply(ZERO))))

	refute("S(S(0)) != S(0)", EQ.
		apply(S.apply(S.apply(ZERO))).
		apply(S.apply(ZERO)),
	)
	refute("S(S(S(0))) != S(S(0))", EQ.
		apply(S.apply(S.apply(S.apply(ZERO)))).
		apply(S.apply(S.apply(ZERO))),
	)

	assert("Number(6) == Number(6)", EQ.apply(Number(6)).apply(Number(6)))
	refute("Number(3) != Number(7)", EQ.apply(Number(3)).apply(Number(7)))

	assert("Number(4) > Number(2)", GRT.apply(Number(4)).apply(Number(2)))
	refute("Number(5) is not greater than Number(5)", GRT.apply(Number(5)).apply(Number(5)))
	refute("Number(8) is not greater than Number(10)", GRT.apply(Number(8)).apply(Number(10)))

	assert("Number(4) == Number(1) + Number(3)", EQ.apply(Number(4)).apply(ADD.apply(Number(1)).apply(Number(3))))
	assert("Number(0) == Number(0) + Number(0)", EQ.apply(Number(0)).apply(ADD.apply(Number(0)).apply(Number(0))))
	assert("Number(6) == Number(0) + Number(6)", EQ.apply(Number(6)).apply(ADD.apply(Number(0)).apply(Number(6))))

	assert("Number(2) == Number(5) - Number(3)", EQ.apply(Number(2)).apply(SUB.apply(Number(5)).apply(Number(3))))
	assert("Number(0) == Number(2) - Number(2)", EQ.apply(Number(0)).apply(SUB.apply(Number(2)).apply(Number(2))))
	assert("Number(0) == Number(3) - Number(4)", EQ.apply(Number(0)).apply(SUB.apply(Number(3)).apply(Number(4))))

	assert("Number(6) == Number(2) * Number(3)", EQ.apply(Number(6)).apply(MUL.apply(Number(2)).apply(Number(3))))
	assert("Number(0) == Number(4) * Number(0)", EQ.apply(Number(0)).apply(MUL.apply(Number(4)).apply(Number(0))))
	assert("Number(0) == Number(0) * Number(4)", EQ.apply(Number(0)).apply(MUL.apply(Number(0)).apply(Number(4))))
	assert("Number(3) == Number(1) * Number(3)", EQ.apply(Number(3)).apply(MUL.apply(Number(1)).apply(Number(3))))
	assert("Number(3) == Number(3) * Number(1)", EQ.apply(Number(3)).apply(MUL.apply(Number(3)).apply(Number(1))))
}

func testLogicalOperators() {
	fmt.Println("BOOLEANS")
	assert("true is true", T)
	refute("false is false", F)
	fmt.Println("")

	fmt.Println("IF")
	assert("if true true else false => true", IF.apply(T).apply(T).apply(F))
	refute("if false true else false => false", IF.apply(F).apply(T).apply(F))
	fmt.Println("")

	fmt.Println("NOT")
	refute("not true => F", NOT.apply(T))
	assert("not false => T", NOT.apply(F))
	fmt.Println("")

	fmt.Println("BOOLEAN EQUALITY")
	assert("true == true => T", BEQ.apply(T).apply(T))
	refute("true == false => F", BEQ.apply(T).apply(F))
	refute("false == true => F", BEQ.apply(F).apply(T))
	assert("false == false => T", BEQ.apply(F).apply(F))
	fmt.Println("")

	fmt.Println("AND")
	assert("true and true => T", AND.apply(T).apply(T))
	refute("true and false => F", AND.apply(T).apply(F))
	refute("false and true => F", AND.apply(F).apply(T))
	refute("false and false => F", AND.apply(F).apply(F))
	fmt.Println("")

	fmt.Println("OR")
	assert("true or true => T", OR.apply(T).apply(T))
	assert("true or false => T", OR.apply(T).apply(F))
	assert("false or true => T", OR.apply(F).apply(T))
	refute("false or false => F", OR.apply(F).apply(F))
	fmt.Println("")

	fmt.Println("NOR")
	refute("true nor true => F", NOR.apply(T).apply(T))
	refute("true nor false => F", NOR.apply(T).apply(F))
	refute("false nor true => F", NOR.apply(F).apply(T))
	assert("false nor false => T", NOR.apply(F).apply(F))
	fmt.Println("")

	fmt.Println("XOR")
	refute("true xor true => F", XOR.apply(T).apply(T))
	assert("true xor false => T", XOR.apply(T).apply(F))
	assert("false xor true => T", XOR.apply(F).apply(T))
	refute("false xor false => F", XOR.apply(F).apply(F))
	fmt.Println("")
}

func main() {
	testLogicalOperators()
	testStructs()
	testNaturalNumbers()
}
