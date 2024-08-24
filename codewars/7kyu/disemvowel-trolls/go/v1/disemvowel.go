//
// tags: string regex vowel replace remove
//

package disemvowel_v1

import "regexp"

var re = regexp.MustCompile(`[AEIOUaeiou]`)

// Disemvowel removes uppercase and lowercase vowels from the
// input string.
//
// NOTE: ‘y’ is not considered a vowel for this exercise.
func Disemvowel(sentence string) (result string) {
	return re.ReplaceAllString(sentence, result)
}
