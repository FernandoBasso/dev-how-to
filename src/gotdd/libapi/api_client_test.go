package libapi

// func TestAPIClient(t *testing.T) {
// 	t.Run("creates an API client", func(t *testing.T) {
// 		// cfg := NewAPIConfig("https://example.org/api")
// 		// _, _ = NewAPIClient(cfg)
// 	})
// }

// type spyHTTPClient struct {
// 	req *http.Request
// 	res *http.Response
// 	err error
// }

// func newSpyHTTPClient(res *http.Response) *spyHTTPClient {
// 	return &spyHTTPClient{
// 		res: res,
// 		err: nil,
// 	}
// }

// func newBrokenHTTPClient(err error) *spyHTTPClient {
// 	return &spyHTTPClient{
// 		res: &http.Response{},
// 		err: err,
// 	}
// }

// func (s *spyHTTPClient) Do(req *http.Request) (*http.Response, error) {
// 	s.req = req
// 	return &http.Response{}, nil
// }
