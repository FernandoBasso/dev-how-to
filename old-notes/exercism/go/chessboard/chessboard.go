package chessboard

// File stores information about which squares are occupied by a piece.
type File []bool

// Chessboard represents the chess board.
type Chessboard map[string]File

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file.
func CountInFile(cb Chessboard, file string) int {
	count := 0

	for _, square := range cb[file] {
		if square {
			count++
		}
	}

	return count
}

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank.
func CountInRank(cb Chessboard, rank int) int {
	count := 0

	for _, file := range cb {
		for idx, sqr := range file {
			if rank-1 == idx && sqr {
				count++
			}
		}
	}

	return count
}

// CountAll should count how many squares are present in the chessboard.
func CountAll(cb Chessboard) int {
	count := 0

	for range cb {
		count++
	}

	return count * count
}

// CountOccupied returns how many squares are occupied in the chessboard.
func CountOccupied(cb Chessboard) int {
	countOccupied := 0

	for _, file := range cb {
		for _, sqr := range file {
			if sqr {
				countOccupied++
			}
		}
	}

	return countOccupied
}
