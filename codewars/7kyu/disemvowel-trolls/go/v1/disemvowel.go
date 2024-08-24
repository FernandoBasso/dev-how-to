//
// tags: string regex vowel replace remove
//

package disemvowel_v1

import "regexp"

var re = regexp.MustCompile(`[AEIOUaeiou]`)

func Disemvowel(sentence string) (result string) {
	return re.ReplaceAllString(sentence, result)
}
