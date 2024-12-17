package libapi

import (
	"bytes"
	_ "embed"
	"errors"
	"io"
	"net/http"
	"testing"

	"github.com/stretchr/testify/require"
)

//go:embed users.json
var usersJSON []byte

func TestUsers(t *testing.T) {
	apiConfig := NewAPIConfig("https://example.org/api")

	t.Run("makes a valid, correct request", func(t *testing.T) {
		fakeResponse := http.Response{Body: toBody([]byte{})}
		spyClient := newSpyUsersClient(&fakeResponse)
		NewUsersClient(apiConfig, spyClient).All()

		expectedRequest := &http.Request{
			URL: apiConfig.URL.JoinPath("users"),
			Header: map[HeaderName][]HeaderValue{
				"Authorization": {"Bearer mock-token"},
				"Content-Type":  {"application/json"},
			},
		}

		require.Equal(t, expectedRequest, spyClient.request)
	})

	t.Run("returns error on non-200 OK response", func(t *testing.T) {
		apiErr := errors.New("Users endpoint replied with a non-200 OK status")
		spyClient := newBrokenUsersClient(apiErr)
		_, err := NewUsersClient(apiConfig, spyClient).All()

		require.EqualError(t, err, err.Error())
	})

	t.Run("can GET users", func(t *testing.T) {
		fakeResponse := http.Response{Body: toBody(usersJSON)}
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
}

type spyUsersClient struct {
	err      error
	request  *http.Request
	response *http.Response
}

func newBrokenUsersClient(err error) *spyUsersClient {
	return &spyUsersClient{
		err:      err,
		response: &http.Response{},
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
