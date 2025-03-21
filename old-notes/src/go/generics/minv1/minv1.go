package main

import (
  "cmp"
  "fmt"
)

// min returns the minimum value.
func min[T cmp.Ordered](x, y T) T {
  if x < y {
    return x
  }

  return y
}

func main() {
  d := min[int8](3, 2)
  s := min[string]("klm", "abc")
  f := min[float64](1.01, 1.1)

  fmt.Println(d)
  // => 2

  fmt.Println(s)
  // => abc

  fmt.Println(f)
  // 1.01
}
