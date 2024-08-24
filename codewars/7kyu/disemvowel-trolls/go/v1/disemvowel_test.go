//
// tags: string regex vowel replace remove
//

package disemvowel_v1

import "testing"

func TestDisemvowel(t *testing.T) {
	t.Run("empty string", func(t *testing.T) {
		got := Disemvowel("")
		want := ""

		if got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})

	t.Run("sentence containing only non-vowels", func(t *testing.T) {
		got := Disemvowel("L337 sp34k")
		want := "L337 sp34k"

		if got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})

	t.Run("sentence containing only vowels", func(t *testing.T) {
		got := Disemvowel("AeiOU")
		want := ""

		if got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})

	t.Run("sentence containing vowels and non vowels", func(t *testing.T) {
		got := Disemvowel("Boo! LOL")
		want := "B! LL"

		if got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})
}
