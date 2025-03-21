package main

import "fmt"

type Wallet struct {
	balance int
}

func (w *Wallet) Deposit(amount int) {
	w.balance += amount
	fmt.Printf("Mem in Deposit(): %p\n", &w.balance)
}

func (w *Wallet) Balance() int {
	return w.balance
}

