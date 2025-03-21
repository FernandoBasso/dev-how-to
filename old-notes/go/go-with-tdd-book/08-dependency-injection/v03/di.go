package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
)

// Greet writes name to the buffer.
func Greet(writer io.Writer, name string) {
	fmt.Fprintf(writer, "Hello, %s.\n", name)
}

func MyGreetHanler(w http.ResponseWriter, r *http.Request) {
	Greet(w, "Aayla Secura")
}

func main() {
	log.Fatal(http.ListenAndServe(":5001", http.HandlerFunc(MyGreetHanler)))
}
