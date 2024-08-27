//
// tags: go exercism switch conditional
//

package blackjack

// ParseCard returns the integer value of a card following
// blackjack ruleset.
//
// NOTE: This function could be writen in more clever ways using a
// look-up table of sorts, but the challenge is mostly about switch case
// statements, so we'll go with it for this solution.
func ParseCard(card string) int {
	switch card {
	case "two":
		return 2
	case "three":
		return 3
	case "four":
		return 4
	case "five":
		return 5
	case "six":
		return 6
	case "seven":
		return 7
	case "eight":
		return 8
	case "nine":
		return 9
	case "ten", "jack", "queen", "king":
		return 10
	case "ace":
		return 11
	default:
		return 0
	}
}

// FirstTurn returns the decision for the first turn, given two cards of
// the player and one card of the dealer.
func FirstTurn(card1, card2, dealerCard string) string {
	sum := ParseCard(card1) + ParseCard(card2)
	switch {
	case card1 == "ace" && card2 == "ace":
		return "P"
	case sum == 21:
		if ParseCard(dealerCard) < 10 {
			return "W"
		}
		return "S"
	case 17 <= sum && sum <= 20:
		return "S"
	case 12 <= sum && sum <= 16:
		if ParseCard(dealerCard) < 7 {
			return "S"
		}
		return "H"
	default:
		return "H"
	}
}
