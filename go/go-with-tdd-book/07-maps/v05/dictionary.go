package main

type Dict map[string]string

const (
	// ErrNotFound means a word's definition could not be found.
	ErrNotFound   = DictErr("word not found")

	// ErrWordExists means a word definition already exists.
	ErrWordExists = DictErr("word already exists")
)

// DictErr is any error that could happen while working with the dictionary.
type DictErr string

/* Implements the error interface */
func (err DictErr) Error() string {
  return string(err)
}

// Search searches for a word definition.
func (dict Dict) Search(word string) (string, error) {
	def, ok := dict[word]

	if !ok {
		return "", ErrNotFound
	}

	return def, nil
}

// Add adds a word definition if it does not already exist.
func (dict Dict) Add(word, definition string) error {
	_, err := dict.Search(word)

	switch err {
	case ErrNotFound:
		dict[word] = definition
	case nil:
		return ErrWordExists
	default:
		return err
	}

	return nil
}

// Update updates an existing definition.
func (dict Dict) Update(word, definition string) error {
	dict[word] = definition
	return nil
}
