package count_vowels_v1

import "strings"

const VOWELS = "AEIOUaeiou"

// CountVowels counts the number of uppercase and lowercase vowels from
// the English alphabet.
//
// ASSUME: "y" is not considered a vowel for this exercise.
func CountVowels(str string) (count int) {
	count = 0

	for _, c := range str {
		if strings.Contains(VOWELS, string(c)) {
			count++
		}
	}

	return count
}
