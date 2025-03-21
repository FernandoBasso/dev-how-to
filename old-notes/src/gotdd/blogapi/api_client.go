package blogapi

import "net/http"

type HTTPClient interface {
	Do(*http.Request) (*http.Response, error)
}

type APIClient struct {
	apiConfig APIConfig
	apiClient HTTPClient
}

func NewAPIClient(apiConfig APIConfig, httpClient HTTPClient) *APIClient {
	return &APIClient{
		apiConfig: apiConfig,
		apiClient: httpClient,
	}
}
