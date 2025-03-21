//
// tags: string regex vowel replace remove
//

package disemvowel_v2

// Disemvowel removes uppercase and lowercase vowels from the
// input string.
//
// NOTE: ‘y’ is not considered a vowel for this exercise.
func Disemvowel(sentence string) (result string) {
	for _, chr := range sentence {
		switch chr {
		case 'A', 'a', 'E', 'e', 'I', 'i', 'O', 'o', 'U', 'u':
			continue
		default:
			/* This is inefficient as the resulting string
			 * gets recreated over and over again. */
			result = result + string(chr)
		}
	}

	return result
}
