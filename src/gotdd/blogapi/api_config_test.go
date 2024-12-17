package blogapi

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestAPIConfig(t *testing.T) {
	t.Run("panics when provided API URL is not parseable", func(t *testing.T) {
		require.Panics(t, func() {
			NewAPIConfig("no-scheme-so-this-is-not-a-conforming-HTTP-URI")
		})
	})

	t.Run("creates valid API config when URL is valid", func(t *testing.T) {
		require.NotPanics(t, func() {
			NewAPIConfig("https://example.org/api")
		})
	})
}
