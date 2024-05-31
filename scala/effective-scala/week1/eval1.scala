package Eval1

//
// The body of a val definition is computed once (no mather if x
// is used or not) and reused in case it is used multiple times.
//
val x = 2 * 2

//
// A method with no params.
//
// The body of a def is evaluated each time f is used. If
// it is never used, it is never evaluated.
//
// def is lazy and defers evaluation until needed.
//
def f = 2 * 2
