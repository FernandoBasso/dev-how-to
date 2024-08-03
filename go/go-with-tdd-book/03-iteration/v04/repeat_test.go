package iteration

import (
	"fmt"
	"testing"
)

func TestRepeat(t *testing.T) {
	t.Run("repeat the default number of times", func(t *testing.T) {
		got := Repeat("a", -1)
		want := "aaaaa"

		if got != want {
			t.Errorf("got %q wanted %q", got, want)
		}
	})

	t.Run("repeat the specified number of times", func(t *testing.T) {
		got := Repeat("z", 3)
		want := "zzz"

		if got != want {
		  t.Errorf("got %q wanted %q", got, want)
		}
	})
}

func ExampleRepeat() {
	repeated := Repeat("z", -1)
	fmt.Println(repeated)
	// Output: zzzzz
}

// Run this test file with:
//
//	$ go test -bench=. or "." on Windows.
func BenchmarkRepeat(b *testing.B) {
	// for i := 0; i < b.N; i++
	for i := 0; i < b.N; i++ {
		Repeat("z", -1)
	}
}
