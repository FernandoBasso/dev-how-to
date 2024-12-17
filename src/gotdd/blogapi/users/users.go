package users

import (
	"blogapi"
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
	config blogapi.APIConfig
	client blogapi.HTTPClient
}

func NewUsersClient(
	apiConfig blogapi.APIConfig,
	apiClient blogapi.HTTPClient,
) *UsersClient {
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
