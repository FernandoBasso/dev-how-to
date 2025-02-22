package router

import (
	"fmt"
)

type Handler func(pageName string)

type Router struct {
	handlers map[string]Handler
}

func New(handlers map[string]Handler) Router {
	return Router{handlers: handlers}
}

func (r *Router) Route(pageName string) error {
	return fmt.Errorf("could not find handler for page %s", pageName)
}
