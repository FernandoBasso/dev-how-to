package cards

// isValidIndex checks if the given index is a valid index
// and within the bounds of the passed slice.
func isValidIndex(slice []int, idx int) bool {
	if idx < 0 || idx >= len(slice) {
		return false
	}

	return true
}

// FavoriteCards returns a slice with the cards 2, 6 and 9 in that order.
func FavoriteCards() []int {
	return []int{2, 6, 9}
}

// GetItem retrieves an item from a slice at given position.
// If the index is out of range, we want it to return -1.
func GetItem(slice []int, index int) int {
	if !isValidIndex(slice, index) {
		return -1
	}

	return slice[index]
}

// SetItem writes an item to a slice at given position overwriting an existing value.
// If the index is out of range the value needs to be appended.
func SetItem(slice []int, index, value int) []int {
	newSlice := slice[:]

	if isValidIndex(newSlice, index) {
		newSlice[index] = value
	} else {
		newSlice = append(newSlice, value)
	}

	return newSlice
}

// PrependItems adds an arbitrary number of values at the front of a slice.
func PrependItems(slice []int, values ...int) []int {
	newSlice := append(values[:], slice...)
	return newSlice
}

// RemoveItem removes an item from a slice by modifying the existing slice.
func RemoveItem(slice []int, index int) []int {
	if !isValidIndex(slice, index) {
		return slice
	}

	newSlice := append(slice[:index], slice[index+1:]...)

	return newSlice
}
