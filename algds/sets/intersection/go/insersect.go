package intersect

func Intersect[T comparable](xs []T, ys []T) []T {
	sect := []T{}
	seen := make(map[T]byte)

	for _, e := range ys {
		seen[e] = 1
	}

	for _, e := range xs {
		if _, ok := seen[e]; ok {
			sect = append(sect, e)
		}
	}

	return sect
}
