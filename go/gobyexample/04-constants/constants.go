package main

import (
  "fmt"
  "math"
)

const s string = "constant string"

func main() {
  fmt.Println(s)

  const n = 500_000_000
  const m = 5e8
  fmt.Println(n, m, n == m)

  const d = 3e20 / n
  fmt.Println(d)

  fmt.Println(int64(d))

  fmt.Println(math.Sin(d))
}
