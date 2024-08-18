package main

import "fmt"

type User struct {
	id   int64
	name string
}

func format(u User) string {
	return fmt.Sprintf("%8s: %5d\n%8s: %5s\n", "id", u.id, "name", u.name)
}

func main() {
	u := User{id: 1, name: "Aayla Secura"}
	fmt.Printf("#%v\n", u)
	fmt.Println(format(u))
}
