package blogapi

import (
	"encoding/json"
	"io"
	"net/http"
)

type IUser interface {
	All() ([]User, error)
}

type User struct {
	Id   int    `json:"id"`
	Name string `json:"name"`
}

type UsersClient struct {
	config APIConfig
	client HTTPClient
}

func NewUsersClient(apiConfig APIConfig, apiClient HTTPClient) *UsersClient {
	return &UsersClient{
		config: apiConfig,
		client: apiClient,
	}
}

func (uc UsersClient) All() ([]User, error) {
	res, err := uc.client.Do(&http.Request{
		URL: uc.config.URL.JoinPath("users"),
		Header: map[string][]string{
			"Authorization": {"Bearer mock-token"},
			"Content-Type":  {"application/json"},
		},
	})

	if err != nil {
		return []User{}, err
	}

	var users []User
	body, _ := io.ReadAll(res.Body)

	_ = json.Unmarshal(body, &users)

	return users, nil
}
