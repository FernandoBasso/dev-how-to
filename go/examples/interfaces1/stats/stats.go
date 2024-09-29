package main

import "fmt"

type Counter interface {
	Add(inc int)
	Value() int
}

type Stats struct {
	value int
}

func (s *Stats) Add(v int) {
	(*s).value += v
}

func (s *Stats) Value() int {
	return (*s).value
}

func main() {
	stats1 := &Stats{}

	stats1.Add(1)
	stats1.Add(10)

	stats2 := &Stats{value: 100}

	fmt.Println(stats1.Value())
	fmt.Println(stats2.Value())
}
