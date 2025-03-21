package lasagna

// PreparationTime returns an estimation of time need to prepare
// the lasagna.
func PreparationTime(layers []string, avgTimePerLayer int) int {
	time := avgTimePerLayer

	if avgTimePerLayer == 0 {
		time = 2
	}

	return len(layers) * time
}

// Quantities calculates the necessary quantities of noodles and
// sauce based on the number of layers.
func Quantities(layers []string) (noodles int, sauce float64) {
	gramsOfNoodles := 0
	littersOfSauce := 0.0
	numLayers := len(layers)

	for i := 0; i < numLayers; i++ {
		if layers[i] == "noodles" {
			gramsOfNoodles += 50
		} else if layers[i] == "sauce" {
			littersOfSauce += 0.2
		}
	}

	return gramsOfNoodles, littersOfSauce
}

// AddSecretIngredient adds the secret ingredient from the friend's
// list to our own list.
func AddSecretIngredient(friendsList []string, myList []string) {
	myList[len(myList)-1] = friendsList[len(friendsList)-1]
}

// ScaleRecipe returns a new slice with the new quantities based on the
// number of portions we want to prepare.
func ScaleRecipe(quantities []float64, numPortions int) []float64 {
	quantitiesLen := len(quantities)
	scaleFactor := float64(numPortions) / 2.0

	newQuantities := make([]float64, quantitiesLen)

	for i := 0; i < quantitiesLen; i++ {
		newQuantities[i] = quantities[i] * scaleFactor
	}

	return newQuantities
}
