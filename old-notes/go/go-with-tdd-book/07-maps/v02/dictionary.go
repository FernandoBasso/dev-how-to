package main

import "errors"

type Dict map[string]string

var ErrNotFound = errors.New("word not found")

func (d Dict) Search(word string) (string, error) {
	def, ok := d[word]

	if !ok {
		return "", ErrNotFound
	}

	return def, nil
}
