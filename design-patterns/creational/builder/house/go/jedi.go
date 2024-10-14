package main

type Jedi struct {
	name   string
	skills []string
	level  int
}

type IJedi interface {
	setName()
	setSkills()
	setLevel()
	getName()
	getSkills()
	getLevel()
}

func (j *Jedi) setName() {
	panic("not implemented") // TODO: Implement
}

func (j *Jedi) setSkills() {
	panic("not implemented") // TODO: Implement
}

func (j *Jedi) setLevel() {
	panic("not implemented") // TODO: Implement
}

func (j *Jedi) getName() {
	panic("not implemented") // TODO: Implement
}

func (j *Jedi) getSkills() {
	panic("not implemented") // TODO: Implement
}

func (j *Jedi) getLevel() {
	panic("not implemented") // TODO: Implement
}
