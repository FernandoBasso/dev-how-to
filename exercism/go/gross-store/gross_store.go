package gross

// Units stores the Gross Store unit measurements.
func Units() map[string]int {
	return map[string]int{
		"quarter_of_a_dozen": 3,
		"half_of_a_dozen":    6,
		"dozen":              12,
		"small_gross":        120,
		"gross":              144,
		"great_gross":        1728,
	}
}

// NewBill creates a new bill.
func NewBill() map[string]int {
	return map[string]int{}
}

// AddItem adds an item to customer bill.
func AddItem(bill, units map[string]int, item, unit string) bool {
	val, exists := units[unit]

	if !exists {
		return false
	}

	// If the item is already there, it contains the int zero
	// value 0, so we can simply increment it.
	bill[item] += val

	return true
}

// RemoveItem removes an item from customer bill.
func RemoveItem(bill, units map[string]int, item, unit string) bool {
	_, item_exists := bill[item]
	_, unit_exists := units[unit]

	if !item_exists || !unit_exists || bill[item]-units[unit] < 0 {
		return false
	}

	if bill[item]-units[unit] == 0 {
		delete(bill, item)
		return true
	}

	bill[item] -= units[unit]

	return true
}

// GetItem returns the quantity of an item that the customer has in
// his/her bill.
func GetItem(bill map[string]int, item string) (int, bool) {
	// `qty` will be the zero value in case the item is not found.
	// `found` will be either `true` or `false`.
	// Therefore, we don't nee to use any conitionals at all. We
	// can simply return those two values.
	qty, found := bill[item]
	return qty, found
}
