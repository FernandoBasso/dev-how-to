package iteration

import "testing"

func TestRepeat(t *testing.T) {
	got := Repeat("a")
	want := "aaaaa"

	if got != want {
		t.Errorf("got %q wanted %q", got, want)
	}
}

// Run this test file with:
//
//	$ go test -bench=. or "." on Windows.
func BenchmarkRepeat(b *testing.B) {
	// for i := 0; i < b.N; i++
	for i := 0; i < b.N; i++ {
		Repeat("z")
	}
}
