package blogapi

import (
	"net/url"
)

type APIURL = string

type APIConfig struct {
	URL url.URL
}

func NewAPIConfig(apiURL APIURL) APIConfig {
	_, err := url.ParseRequestURI(apiURL)

	if err != nil {
		panic("TODO: test API url")
		// panic("Failed to parse give API URL")
	}

	return APIConfig{}
}
