class Dog
  attr_reader :age, :dog_years

  def dog_years=(years)
    @dot_years = years
  end

  def age=(years)
    @age = years
    # Would consider dog_years an lvar.
    #dog_years = years * 7
    # So, we use self, and self inside the this instance method
    # refers to the instance object.
    self.dog_years = years * 7
  end

  private :dog_years=
end

#
# Ruby implements private methods by means of not allowing an explicit
# receiver. Except that with writter/setter methods, not setting an
# explicit receiver makes an assignment become an assignment to a local
# variable. Thus, that rule of not allowing an explicit receiver doesn't
# apply for writter methodos. And the receiver can and must only be the
# keyword `self'. You can't even assign `self' to another variable and
# use that variable. It has to be the keyword `self'.
#
