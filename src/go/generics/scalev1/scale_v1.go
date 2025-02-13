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

	fmt.Printf("%s\n", scaledXs.Str())
	// ~ scaledXs.Str undefined (type []int32 has no field or method Str)
}
