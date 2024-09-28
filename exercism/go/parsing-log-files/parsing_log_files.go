package parsinglogfiles

import "regexp"

func IsValidLine(text string) bool {
	re := regexp.MustCompile(`^\[(TRC|DBG|INF|WRN|ERR|FTL)\]`)
	return re.MatchString(text)
}

func SplitLogLine(text string) []string {
	re := regexp.MustCompile(`<[^>\w]*>`)
	return re.Split(text, -1)
}

func CountQuotedPasswords(lines []string) int {
	re := regexp.MustCompile(`(i?)".*password.*"`)
	count := 0

	for _, line := range lines {
		count += len(re.FindStringSubmatch(line))
	}

	return count
}

func RemoveEndOfLineText(text string) string {
	re := regexp.MustCompile(`end-of-line\d+`)
	return re.ReplaceAllString(text, "")
}

func TagWithUserName(lines []string) []string {
	re := regexp.MustCompile(`User +([^ ]+)`)
	out := []string{}

	for _, line := range lines {
		match := re.FindStringSubmatch(line)

		if match == nil {
			out = append(out, line)
		} else {
			out = append(out, "[USR] "+match[1]+" "+line)
		}
	}

	return out
}
