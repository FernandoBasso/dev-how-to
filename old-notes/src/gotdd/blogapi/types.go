package blogapi

// HeaderName represents header names. Examples:
//
//	var auth HeaderName = "Authorization"
//	contentType := HeaderName("ContentType")
type HeaderName = string

// HeaderValue represents header values. Examples:
//
//	var tok HeaderValue = "Bearer token"
//	typeJson := HeaderValue("application/json")
//
// But remember that http.Request.Header requires header values to be
// slices. So we'd most likely use HeaderValue like this:
//
//	var tok []HeaderValue = {"Bearer token"}
//	typeJSON := []HeaderValue{"application/json"}
//
// A full example could be something like this:
//
//	req := http.Request{
//		Header: map[HeaderName][]HeaderValue{
//			"Authorization": {"Bearer mock-token"},
//			"Content-Type":  {"application/json"},
//		},
//	}
type HeaderValue = string
