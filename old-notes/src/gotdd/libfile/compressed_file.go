package libfile

type BytesCompressionFunc = func([]byte) []byte

type CompressedFile struct {
	underlyingFile  File
	compressionFunc BytesCompressionFunc
}

func NewCompressedFile(file File, fn BytesCompressionFunc) *CompressedFile {
	return &CompressedFile{
		underlyingFile:  file,
		compressionFunc: fn,
	}
}

/*
 * Content() is compressing before returning the _compressed_ content. I
 * wonder if we should store the compressed file content instead of just
 * returning it.
 *
 * Is it OK that the content is compressed every time we call Content()?
 * Sure one would call it only when strictly necessary, but it still
 * feels like we could memoize this invocation.
 */
func (c *CompressedFile) Content() []byte {
	return c.compressionFunc(c.underlyingFile.Content())
}
