package router_test

import (
	"fmt"
	"router"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestFnRouter(t *testing.T) {
	t.Run("should return error when handler cannot be found", func(t *testing.T) {
		spy := []string{}
		router := router.New(map[string]router.Handler{})

		err := router.Route("non-existing-page")

		require.Empty(t, spy)

		require.EqualError(
			t,
			err,
			fmt.Sprintf("could not find handler for page %s", "non-existing-page"),
		)
	})

	t.Run("correctly routes to teh appropriate handler", func(t *testing.T) {
		spy := []string{}
		router := router.New(map[string]router.Handler{
			"home": func(page string) {
				spy = append(spy, page+" handled")
			},
			"about": func(page string) {
				spy = append(spy, page+" handled")
			},
		})

		err := router.Route("home")

		require.NoError(t, err)

		require.Equal(t, []string{"home handled"}, spy)
	})
}
