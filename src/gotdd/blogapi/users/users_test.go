package users

import (
	"bytes"
	_ "embed"
	"errors"
	"io"
	"net/http"
	"testing"

	"blogapi"

	"github.com/stretchr/testify/require"
)

//go:embed users.json
var usersJSON []byte

func TestUsers(t *testing.T) {
	apiConfig := blogapi.NewAPIConfig("https://example.org/api")
	t.Run("All()", func(t *testing.T) {
		t.Run("makes a valid, correct request", func(t *testing.T) {
			fakeResponse := http.Response{Body: toBody([]byte{})}
			spyClient := newSpyUsersClient(&fakeResponse)
			NewUsersClient(apiConfig, spyClient).All()

			expectedRequest := &http.Request{
				URL: apiConfig.URL.JoinPath("users"),
				Header: map[blogapi.HeaderName][]blogapi.HeaderValue{
					"Authorization": {"Bearer mock-token"},
					"Content-Type":  {"application/json"},
				},
			}

			require.Equal(t, expectedRequest, spyClient.request)
		})

		t.Run("when there is a network failure or some sort", func(t *testing.T) {
			apiErr := errors.New("network failure")
			spyClient := newBrokenUsersClient(http.Response{}, apiErr)
			_, err := NewUsersClient(apiConfig, spyClient).All()

			require.EqualError(t, err, err.Error())
		})

		t.Run("when API returns a non-200 OK status", func(t *testing.T) {
			spyClient := newBrokenUsersClient(http.Response{
				Status:     http.StatusText(http.StatusForbidden),
				StatusCode: http.StatusForbidden,
			},
				nil,
			)

			usersClient := NewUsersClient(apiConfig, spyClient)
			_, err := usersClient.All()

			require.Error(t, err, "Unexpected server response")
		})

		t.Run("can GET users", func(t *testing.T) {
			fakeResponse := http.Response{
				Status:     http.StatusText(http.StatusOK),
				StatusCode: http.StatusOK,
				Body:       toBody(usersJSON),
			}
			spyClient := newSpyUsersClient(&fakeResponse)
			usersClient := NewUsersClient(apiConfig, spyClient)
			users, err := usersClient.All()

			expectedUsers := []User{
				{Id: 1, Name: "Aayla Secura"},
				{Id: 2, Name: "Ahsoka Tano"},
			}

			require.NoError(t, err)
			require.Equal(t, expectedUsers, users)
		})
	})
}

type spyUsersClient struct {
	err      error
	request  *http.Request
	response *http.Response
}

func newBrokenUsersClient(res http.Response, err error) *spyUsersClient {
	return &spyUsersClient{
		err:      err,
		response: &res,
	}
}

func newSpyUsersClient(response *http.Response) *spyUsersClient {
	return &spyUsersClient{
		err:      nil,
		response: response,
	}
}

func (s *spyUsersClient) Do(req *http.Request) (*http.Response, error) {
	if s.err != nil {
		return s.response, s.err
	}

	s.request = req

	return s.response, nil
}

func toBody(b []byte) io.ReadCloser {
	return io.NopCloser(bytes.NewReader(b))
}
