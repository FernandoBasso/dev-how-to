package libhouse

type Builder interface {
	setWindowType(windowType string)
	setDoorType(doorType string)
	setFloors(floors int)
	getHouse() House
}

// getBuilder gets a pointer to a builder. Builders we currently support
// are "normal" and "igloo".

// Returns nil if an unknown kind is passed.
func getBuilder(kind string) Builder {
	switch kind {
	case "normal":
		return newNormalBuilder()
	case "igloo":
		return newIglooBuilder()
	default:
		return nil
	}
}
