package main

import (
	"fmt"

	"golang.org/x/exp/constraints"
)

// scale takes a slice of Integer and returns a new slice with each
// integer multiplied by k.
func scale[S ~[]E, E constraints.Integer](s S, k E) S {
	scaled := make(S, len(s))

	for i, v := range s {
		scaled[i] = v * k
	}

	return scaled
}

type Point []int32

func (p Point) Str() string {
	var s string

	for _, v := range p {
		s += string(v) + ", "
	}

	return s
}

func main() {
	xs := Point{2, 3, 4}

	// The type of scaledXs now is Point, which is what we want, and
	// not []int32, which caused problems in the previous version.
	scaledXs := scale(xs, 2)

	fmt.Printf("%s\n", scaledXs.Str())
}

/*

We introduced a new type constraint S ~[]E (which is the type of the
argument), so that the the underlying type of its argument must be a
slice of some element of type E.

So now the first argument of the function is of type S, rather than []E.

But now if we call scale(p, k), and p is of type Point, then the return
type will also be of type Point, and Point does have a method Str.

*/
