package main

import (
	"testing"
	"fmt"
)

func TestWallet(t *testing.T) {
	wallet := Wallet{}
	wallet.Deposit(10)

	fmt.Printf("Mem in test: %p\n", &wallet.balance)

	got := wallet.Balance()
	want := 10

	if got != want {
		t.Errorf("got %d, want %d", got, want)
	}
}
