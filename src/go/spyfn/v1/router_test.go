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
		router := router.New(map[string]router.Handler{
			"home": func(page string) {
				spy = append(spy, "home handled")
			},
			"about": func(page string) {
				spy = append(spy, "about handled")
			},
		})

		err := router.Route("home")

		require.EqualError(
			t,
			err,
			fmt.Sprintf("could not find handler for page %s", "home"),
		)
	})
}
