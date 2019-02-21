# Private, protected, public methods

class Foo
  @count = 0;

  def initialize(num)
    @count  = num
  end

  def count
    @count
  end
end

foo = Foo.new(9);
p foo.count
# → 9, not 0.


p Foo.count
