package count_vowels_v1

import "testing"

func TestCountVowels(t *testing.T) {
  t.Run("empty string", func(t *testing.T) {
    got := CountVowels("")
    want := 0

    if got != want {
      t.Errorf("got count of %d, want count of %d", got, want)
    }
  })

  t.Run("string with no wowels", func(t *testing.T) {
    got := CountVowels("_xyz_ !")
    want := 0

    if got != want {
      t.Errorf("got count of %d, want count of %d", got, want)
    }
  })

  t.Run("string with a few vowels", func(t *testing.T) {
    got := CountVowels("ab cd ef")
    want := 2

    if got != want {
      t.Errorf("got count of %d, want count of %d", got, want)
    }
  })

  t.Run("string with only vowels", func(t *testing.T) {
    got := CountVowels("aAeEiIoOuU")
    want := 10

    if got != want {
      t.Errorf("got count of %d, want count of %d", got, want)
    }
  })
}
