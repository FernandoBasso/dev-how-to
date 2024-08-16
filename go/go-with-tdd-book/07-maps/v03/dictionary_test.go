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

func TestAdd(t *testing.T) {
	dict := Dict{}
	word := "test"
	definition := "added"
	
	dict.Add(word, definition)

	assertDefinition(t, dict, word, definition)
}

func assertStrings(t testing.TB, got, want string) {
	t.Helper()

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

func assertDefinition(t testing.TB, dict Dict, word, definition string) {
  t.Helper()

	got, err := dict.Search(word)

	if err != nil {
		t.Fatal("should find added word:", err)
	}

	assertStrings(t, got, definition);
}
