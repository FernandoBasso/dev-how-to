package count_vowels_v2

// CountVowels counts the number of uppercase and lowercase vowels from
// the English alphabet.
//
// ASSUME: "y" is not considered a vowel for this exercise.
func CountVowels(str string) (count int) {
	count = 0

	for _, c := range str {
		switch c {
		case 'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U':
			count++
		}
	}

	return count
}
