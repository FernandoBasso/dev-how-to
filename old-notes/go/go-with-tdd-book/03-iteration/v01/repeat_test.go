package iteration

import "testing"

func TestRepeat(t *testing.T) {
	got := Repeat("a")
	want := "aaaaa"

	if got != want {
	  t.Errorf("got %q wanted %q", got, want)
	}
}
