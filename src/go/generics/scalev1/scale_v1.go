package main

import (
	"fmt"

	"golang.org/x/exp/constraints"
)

// scale takes a slice of Integer and returns a new slice with each
// integer multiplied by k.
func scale[E constraints.Integer](s []E, k E) []E {
	scaled := make([]E, len(s))

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

	scaledXs := scale(xs, 2)

	// ERROR: Doesn't compile.
	fmt.Printf("%s\n", scaledXs.Str())
	// ~ scaledXs.Str undefined (type []int32 has no field or method Str)
}

/*

The problem is that scale returns a []E, where E is the element type of
the argument slice.

When we call scale() with a value of type Point, whose underlying type
is []int32, we get back a value of type []int32 (not a value of type
Point).

Point has the method Str, but []int32 does not, thus the error.

NOTE: This is one more reason why I generally prefer explicit type
annotations. So instead of:

    v := someFn(x)

We would do something like this:

		var v SomeType = someFn(x)

If somFn() does NOT return SomeType, we know immediately.

Explicit type annotations make the expected type immediately visible,
and we don't need to be in an editor/IDE with LSP or some other tool to
help with hovers or whatever to inspect the returned types. It becomes
visible and explicit even in a plain text file, or in a source control
vendor like Gitlab.

*/
