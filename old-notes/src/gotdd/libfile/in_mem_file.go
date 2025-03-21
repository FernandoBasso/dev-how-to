package libfile

type InMemFile struct {
	content []byte
}

func NewInMemFile(content []byte) *InMemFile {
	return &InMemFile{
		content: content,
	}
}

func (f *InMemFile) Content() (content []byte) {
	return f.content
}
