package main

import "testing"

func TestSearch(t *testing.T) {
	dict := Dict{"test": "just a test"}

	t.Run("known word", func(t *testing.T) {
		got, _ := dict.Search("test")
		want := "just a test"

		assertStrings(t, got, want)
	})

	t.Run("unknown word", func(t *testing.T) {
		_, err := dict.Search("unknown")

		if err == nil {
		  t.Errorf("expected an error but didn't get one")
		}

		assertError(t, err, ErrNotFound)
	})
}

func assertStrings(t testing.TB, got, want string) {
	t.Helper()

	if got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}

func assertError(t testing.TB, got, want error) {
  t.Helper()

	if got != want {
	  t.Errorf("got error %q, want %q", got, want)
	}
}
