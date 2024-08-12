package main

import "testing"

func TestWallet(t *testing.T) {
	wallet := Wallet{}

	wallet.Deposit(10)

	got := wallet.Balance()
	want := Bitcoin(20)

	if got != want {
		t.Errorf("got %s, want %s", got, want)
	}
}

//
// Now that balance is Bitcoin, and Bitcoin has a custom String()
// method, then %s causes that method to be used.
//

